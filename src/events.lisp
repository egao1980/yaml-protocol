(in-package #:yaml-protocol)

;;; Event stream (yaml-test-suite DSL) + compose.
;;; Parser emits events; compose builds the Lisp graph. Aliases / << / Core
;;; schema are compose-time. :object-class is post-compose (see protocol.lisp).

(defstruct (yaml-event (:constructor make-yaml-event))
  (kind :scalar :type keyword)
  (implicit t)
  (flow-p nil)
  (anchor nil)
  (tag nil)
  (style :plain)
  (value nil))

(defun hex-digit-p (c)
  (and c (or (char<= #\0 c #\9)
             (char<= #\a c #\f)
             (char<= #\A c #\F))))

(defun %core-integer (s)
  (let ((sign 1)
        (start 0)
        (n (length s)))
    (when (zerop n)
      (return-from %core-integer nil))
    (cond
      ((char= (char s 0) #\+) (setf start 1))
      ((char= (char s 0) #\-) (setf start 1 sign -1)))
    (when (>= start n)
      (return-from %core-integer nil))
    (cond
      ((and (>= (- n start) 3)
            (char= (char s start) #\0)
            (or (char= (char s (1+ start)) #\x)
                (char= (char s (1+ start)) #\X)))
       (let ((rest (subseq s (+ start 2))))
         (when (and (plusp (length rest))
                    (every #'hex-digit-p rest))
           (* sign (parse-integer rest :radix 16)))))
      ((and (>= (- n start) 3)
            (char= (char s start) #\0)
            (or (char= (char s (1+ start)) #\o)
                (char= (char s (1+ start)) #\O)))
       (let ((rest (subseq s (+ start 2))))
         (when (and (plusp (length rest))
                    (every (lambda (c) (char<= #\0 c #\7)) rest))
           (* sign (parse-integer rest :radix 8)))))
      ((and (char= (char s start) #\0) (= n (1+ start)))
       0)
      ((and (char<= #\1 (char s start) #\9)
            (every #'digit-char-p (subseq s start)))
       (* sign (parse-integer s :start start)))
      ((and (char= (char s start) #\0)
            (every #'digit-char-p (subseq s start))
            (= n (1+ start)))
       0)
      (t nil))))

(defun %core-float (s)
  (let ((n (length s)))
    (when (zerop n)
      (return-from %core-float nil))
    (flet ((inf-p (x)
             (or (string= x ".inf") (string= x ".Inf") (string= x ".INF")
                 (string= x "+.inf") (string= x "+.Inf") (string= x "+.INF")))
           (ninf-p (x)
             (or (string= x "-.inf") (string= x "-.Inf") (string= x "-.INF")))
           (nan-p (x)
             (or (string= x ".nan") (string= x ".NaN") (string= x ".NAN"))))
      (cond
        ((inf-p s)
         (let ((s (find-symbol "DOUBLE-FLOAT-POSITIVE-INFINITY")))
           (if (and s (boundp s)) (symbol-value s) most-positive-double-float)))
        ((ninf-p s)
         (let ((s (find-symbol "DOUBLE-FLOAT-NEGATIVE-INFINITY")))
           (if (and s (boundp s)) (symbol-value s) most-negative-double-float)))
        ((nan-p s)
         (let ((s (find-symbol "DOUBLE-FLOAT-NAN")))
           (if (and s (boundp s)) (symbol-value s) nil)))
        (t
         (when (and (find-if (lambda (c)
                               (or (char= c #\.) (char= c #\e) (char= c #\E)))
                             s)
                    (every (lambda (c)
                             (or (digit-char-p c)
                                 (member c '(#\+ #\- #\. #\e #\E))))
                           s))
           (let* ((*read-default-float-format* 'double-float)
                  (*read-eval* nil))
             (ignore-errors
               (let ((v (read-from-string s)))
                 (and (numberp v) (float v 1.0d0)))))))))))

(defun resolve-plain (s)
  "YAML 1.2 Core schema on a plain scalar (not 1.1 — NO is a string)."
  (cond
    ((or (string= s "")
         (string= s "~")
         (string= s "null") (string= s "Null") (string= s "NULL"))
     :null)
    ((or (string= s "true") (string= s "True") (string= s "TRUE")) t)
    ((or (string= s "false") (string= s "False") (string= s "FALSE")) nil)
    (t
     (or (%core-integer s)
         (%core-float s)
         s))))

(defun %event-escape (s)
  (with-output-to-string (o)
    (loop for c across s
          do (case c
               (#\\ (write-string "\\\\" o))
               (#\Nul (write-string "\\0" o))
               (#\Backspace (write-string "\\b" o))
               (#\Newline (write-string "\\n" o))
               (#\Return (write-string "\\r" o))
               (#\Tab (write-string "\\t" o))
               (t (write-char c o))))))

(defun %style-char (style)
  (ecase style
    (:plain #\:)
    (:single #\')
    (:double #\")
    (:literal #\|)
    (:folded #\>)))

(defun format-event (event &optional (stream *standard-output*))
  (let ((kind (yaml-event-kind event)))
    (ecase kind
      (:stream-start (write-string "+STR" stream))
      (:stream-end (write-string "-STR" stream))
      (:document-start
       (write-string "+DOC" stream)
       (unless (yaml-event-implicit event)
         (write-string " ---" stream)))
      (:document-end
       (write-string "-DOC" stream)
       (unless (yaml-event-implicit event)
         (write-string " ..." stream)))
      (:mapping-start
       (write-string "+MAP" stream)
       (when (yaml-event-flow-p event)
         (write-string " {}" stream))
       (when (yaml-event-anchor event)
         (format stream " &~A" (yaml-event-anchor event)))
       (when (yaml-event-tag event)
         (format stream " <~A>" (yaml-event-tag event))))
      (:sequence-start
       (write-string "+SEQ" stream)
       (when (yaml-event-flow-p event)
         (write-string " []" stream))
       (when (yaml-event-anchor event)
         (format stream " &~A" (yaml-event-anchor event)))
       (when (yaml-event-tag event)
         (format stream " <~A>" (yaml-event-tag event))))
      (:mapping-end (write-string "-MAP" stream))
      (:sequence-end (write-string "-SEQ" stream))
      (:scalar
       (write-string "=VAL" stream)
       (when (yaml-event-anchor event)
         (format stream " &~A" (yaml-event-anchor event)))
       (when (yaml-event-tag event)
         (format stream " <~A>" (yaml-event-tag event)))
       (write-char #\Space stream)
       (write-char (%style-char (yaml-event-style event)) stream)
       (write-string (%event-escape (or (yaml-event-value event) "")) stream))
      (:alias
       (format stream "=ALI *~A" (yaml-event-value event)))))
  (values))

(defun format-events (events)
  "Render EVENTS as yaml-test-suite test.event text (trailing newline)."
  (with-output-to-string (o)
    (dolist (ev (coerce events 'list))
      (format-event ev o)
      (write-char #\Newline o))))

(defun stringify-key (key)
  (cond
    ((stringp key) key)
    ((eq key :null) "null")
    ((eq key t) "true")
    ((null key) "false")
    ((numberp key) (princ-to-string key))
    (t key)))

(defun merge-mapping (dest src)
  "YAML merge key <<. Existing keys win."
  (cond
    ((hash-table-p src)
     (maphash (lambda (k v)
                (unless (nth-value 1 (gethash k dest))
                  (setf (gethash k dest) v)))
              src))
    ((and (vectorp src) (not (stringp src)))
     (loop for i from (1- (length src)) downto 0
           do (merge-mapping dest (aref src i))))
    (t (error 'yaml-parse-error :message "<< merge value must be a mapping"))))

(defun assign-map-entry (ht key val)
  (let ((k (stringify-key key)))
    (if (and (stringp k) (string= k "<<"))
        (merge-mapping ht val)
        (setf (gethash k ht) val))))

(defun %resolve-scalar (event)
  (let* ((raw (or (yaml-event-value event) ""))
         (tag (yaml-event-tag event))
         (style (yaml-event-style event)))
    (cond
      ((null tag)
       (if (eq style :plain)
           (resolve-plain raw)
           raw))
      ((or (string= tag "!")
           (string= tag "tag:yaml.org,2002:str"))
       (if (and (string= tag "!") (eq style :plain))
           (resolve-plain raw)
           raw))
      ((string= tag "tag:yaml.org,2002:null")
       :null)
      ((string= tag "tag:yaml.org,2002:bool")
       (cond
         ((or (string= raw "true") (string= raw "True") (string= raw "TRUE")) t)
         ((or (string= raw "false") (string= raw "False") (string= raw "FALSE")) nil)
         (t raw)))
      ((string= tag "tag:yaml.org,2002:int")
       (or (%core-integer raw) (ignore-errors (parse-integer raw)) raw))
      ((string= tag "tag:yaml.org,2002:float")
       (or (%core-float raw) raw))
      (t
       (if (eq style :plain)
           (resolve-plain raw)
           raw)))))

(defstruct composer
  (events #() :type vector)
  (index 0 :type fixnum)
  (anchors nil))

(defun %c-peek (c)
  (when (< (composer-index c) (length (composer-events c)))
    (yaml-event-kind (aref (composer-events c) (composer-index c)))))

(defun %c-next (c)
  (when (>= (composer-index c) (length (composer-events c)))
    (error 'yaml-parse-error :message "unexpected end of event stream"))
  (prog1 (aref (composer-events c) (composer-index c))
    (incf (composer-index c))))

(defun %c-expect (c kind)
  (let ((ev (%c-next c)))
    (unless (eq (yaml-event-kind ev) kind)
      (error 'yaml-parse-error
             :message (format nil "expected ~A got ~A" kind (yaml-event-kind ev))))
    ev))

(defun %c-bind (c ev object)
  (when (yaml-event-anchor ev)
    (setf (gethash (yaml-event-anchor ev) (composer-anchors c)) object))
  object)

(defun %compose-node (c)
  (let ((ev (%c-next c)))
    (ecase (yaml-event-kind ev)
      (:alias
       (let ((val (gethash (yaml-event-value ev) (composer-anchors c) :missing)))
         (when (eq val :missing)
           (error 'yaml-parse-error
                  :message (format nil "unknown alias *~A" (yaml-event-value ev))))
         val))
      (:scalar
       (%c-bind c ev (%resolve-scalar ev)))
      (:sequence-start
       (let ((items (make-array 0 :adjustable t :fill-pointer 0)))
         (%c-bind c ev items)
         (loop until (eq (%c-peek c) :sequence-end)
               do (vector-push-extend (%compose-node c) items))
         (%c-expect c :sequence-end)
         items))
      (:mapping-start
       (let ((ht (make-hash-table :test #'equal)))
         (%c-bind c ev ht)
         (loop until (eq (%c-peek c) :mapping-end)
               do (assign-map-entry ht (%compose-node c) (%compose-node c)))
         (%c-expect c :mapping-end)
         ht)))))

(defun compose-events (events &key all)
  "Build Lisp values from an event stream.
   Aliases are EQ to the anchored object. Collections are registered
   before they are filled so cycles work. ALL true → vector of documents."
  (let ((c (make-composer :events (coerce events 'vector)
                          :anchors (make-hash-table :test #'equal)))
        (docs '()))
    (%c-expect c :stream-start)
    (loop
      (let ((k (%c-peek c)))
        (cond
          ((or (null k) (eq k :stream-end))
           (return))
          ((eq k :document-start)
           (%c-next c)
           (setf (composer-anchors c) (make-hash-table :test #'equal))
           (if (eq (%c-peek c) :document-end)
               (push :null docs)
               (push (%compose-node c) docs))
           (%c-expect c :document-end))
          (t
           (error 'yaml-parse-error
                  :message (format nil "unexpected event ~A" k))))))
    (when (eq (%c-peek c) :stream-end)
      (%c-next c))
    (setf docs (nreverse docs))
    (if all
        (coerce docs 'vector)
        (if docs
            (first docs)
            :null))))
