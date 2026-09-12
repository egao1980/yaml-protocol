(in-package #:yaml-protocol)

;;; Event stream (yaml-test-suite DSL) + compose.
;;; Parser emits events; compose builds the Lisp graph. Aliases / << / Core
;;; schema are compose-time. :object-class is post-compose (see protocol.lisp).

(defstruct (yaml-event
            (:constructor make-yaml-event)
            (:constructor %yaml-event (kind implicit flow-p anchor tag style value)))
  (kind :scalar :type keyword)
  (implicit t)
  (flow-p nil)
  (anchor nil)
  (tag nil)
  (style :plain)
  (value nil))

(declaim (inline hex-digit-p))

(defun hex-digit-p (c)
  (declare (optimize (speed 3) (safety 1)))
  (and c (or (char<= #\0 c #\9)
             (char<= #\a c #\f)
             (char<= #\A c #\F))))

(defun %all-hex-p (s start end)
  (declare (type string s) (type fixnum start end)
           (optimize (speed 3) (safety 1)))
  (when (>= start end)
    (return-from %all-hex-p nil))
  (loop for i from start below end
        always (hex-digit-p (char s i))))

(defun %all-oct-p (s start end)
  (declare (type string s) (type fixnum start end)
           (optimize (speed 3) (safety 1)))
  (when (>= start end)
    (return-from %all-oct-p nil))
  (loop for i from start below end
        always (char<= #\0 (char s i) #\7)))

(defun %all-digits-p (s start end)
  (declare (type string s) (type fixnum start end)
           (optimize (speed 3) (safety 1)))
  (loop for i from start below end
        always (char<= #\0 (char s i) #\9)))

(defun %core-integer (s)
  (declare (type string s) (optimize (speed 3) (safety 1)))
  (let ((sign 1)
        (start 0)
        (n (length s)))
    (declare (type fixnum start n) (type (integer -1 1) sign))
    (when (zerop n)
      (return-from %core-integer nil))
    (cond
      ((char= (char s 0) #\+) (setf start 1))
      ((char= (char s 0) #\-) (setf start 1 sign -1)))
    (when (>= start n)
      (return-from %core-integer nil))
    (cond
      ((and (>= (the fixnum (- n start)) 3)
            (char= (char s start) #\0)
            (or (char= (char s (1+ start)) #\x)
                (char= (char s (1+ start)) #\X)))
       (let ((from (the fixnum (+ start 2))))
         (when (%all-hex-p s from n)
           (* sign (parse-integer s :start from :end n :radix 16)))))
      ((and (>= (the fixnum (- n start)) 3)
            (char= (char s start) #\0)
            (or (char= (char s (1+ start)) #\o)
                (char= (char s (1+ start)) #\O)))
       (let ((from (the fixnum (+ start 2))))
         (when (%all-oct-p s from n)
           (* sign (parse-integer s :start from :end n :radix 8)))))
      ((and (char= (char s start) #\0) (= n (1+ start)))
       0)
      ((and (char<= #\1 (char s start) #\9)
            (%all-digits-p s start n))
       (* sign (parse-integer s :start start :end n)))
      ((and (char= (char s start) #\0)
            (%all-digits-p s start n)
            (= n (1+ start)))
       0)
      (t nil))))

(defun %core-float (s)
  (declare (type string s) (optimize (speed 3) (safety 1)))
  (let ((n (length s)))
    (declare (type fixnum n))
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
         (let ((dot-or-exp nil)
               (ok t))
           (loop for i from 0 below n
                 for c = (char s i)
                 do (cond
                      ((or (char= c #\.) (char= c #\e) (char= c #\E))
                       (setf dot-or-exp t))
                      ((or (char<= #\0 c #\9)
                           (char= c #\+) (char= c #\-)))
                      (t (setf ok nil))))
           (when (and ok dot-or-exp)
             (let* ((*read-default-float-format* 'double-float)
                    (*read-eval* nil))
               (ignore-errors
                 (let ((v (read-from-string s)))
                   (and (numberp v) (float v 1.0d0))))))))))))

(defun resolve-plain (s)
  "YAML 1.2 Core schema on a plain scalar (not 1.1 — NO is a string)."
  (declare (type string s) (optimize (speed 3) (safety 1)))
  (let ((n (length s)))
    (declare (type fixnum n))
    (when (zerop n)
      (return-from resolve-plain :null))
    (let ((c (char s 0)))
      (cond
        ((or (char<= #\0 c #\9) (char= c #\+) (char= c #\-) (char= c #\.))
         (or (%core-integer s) (%core-float s) s))
        ((or (char= c #\~)
             (and (= n 4) (or (string= s "null") (string= s "Null")
                              (string= s "NULL") (string= s "true")
                              (string= s "True") (string= s "TRUE"))))
         (if (or (char= c #\~) (char= c #\n) (char= c #\N))
             :null
             t))
        ((and (= n 5) (or (string= s "false") (string= s "False")
                          (string= s "FALSE")))
         nil)
        (t s)))))

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
    (map nil (lambda (ev)
               (format-event ev o)
               (write-char #\Newline o))
         events)))

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

(defun %resolve-scalar-raw (raw tag style)
  (let ((raw (or raw "")))
    (cond
      ((null tag)
       (if (eq style :plain)
           (resolve-plain raw)
           raw))
      ;; [100] c-non-specific-tag `!` → tag:yaml.org,2002:str (JSON/Core).
      ((or (string= tag "!")
           (string= tag "tag:yaml.org,2002:str"))
       raw)
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

(defun %resolve-scalar (event)
  (%resolve-scalar-raw (yaml-event-value event)
                       (yaml-event-tag event)
                       (yaml-event-style event)))

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
  (let ((c (make-composer :events (if (and (vectorp events) (not (stringp events)))
                                      events
                                      (coerce events 'vector))
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

;;; Live compose (decode only). Events never materialize. Lookaheads must
;;; nil ys-live and scratch on the event vector (with-ys-checkpoint).

(defstruct (lframe (:constructor %lframe (kind container)))
  (kind :seq :type keyword)
  container
  (want-key t)
  pending-key)

(defstruct (live-composer (:constructor %make-live-composer))
  (stack nil)
  (anchors nil)
  (docs nil)
  (root :empty)
  (hint 16 :type fixnum))

(defun make-live-composer (&optional (hint 16))
  (%make-live-composer :anchors (make-hash-table :test #'equal :size 8)
                       :hint (max 8 hint)))

(declaim (inline live-bind live-place))

(defun live-bind (live object anchor)
  (when anchor
    (setf (gethash anchor (live-composer-anchors live)) object))
  object)

(defun live-place (live object)
  (declare (optimize (speed 3) (safety 1)))
  (let ((frame (car (live-composer-stack live))))
    (cond
      ((null frame)
       (setf (live-composer-root live) object))
      ((eq (lframe-kind frame) :seq)
       (vector-push-extend object (lframe-container frame)))
      (t
       (if (lframe-want-key frame)
           (setf (lframe-pending-key frame) object
                 (lframe-want-key frame) nil)
           (progn
             (assign-map-entry (lframe-container frame)
                               (lframe-pending-key frame)
                               object)
             (setf (lframe-want-key frame) t
                   (lframe-pending-key frame) nil))))))
  object)

(defun live-doc-start (live)
  (clrhash (live-composer-anchors live))
  (setf (live-composer-root live) :empty
        (live-composer-stack live) nil))

(defun live-doc-end (live)
  (when (live-composer-stack live)
    (error 'yaml-parse-error :message "unclosed collection at document end"))
  (push (if (eq (live-composer-root live) :empty)
            :null
            (live-composer-root live))
        (live-composer-docs live))
  (setf (live-composer-root live) :empty))

(defun live-scalar (live value anchor tag style)
  (live-place live (live-bind live (%resolve-scalar-raw value tag style) anchor)))

(defun live-alias (live name)
  (let ((val (gethash name (live-composer-anchors live) :missing)))
    (when (eq val :missing)
      (error 'yaml-parse-error
             :message (format nil "unknown alias *~A" name)))
    (live-place live val)))

(defun live-seq-start (live anchor)
  (let ((items (make-array (live-composer-hint live)
                           :adjustable t :fill-pointer 0)))
    (live-bind live items anchor)
    (live-place live items)
    (push (%lframe :seq items) (live-composer-stack live))))

(defun live-seq-end (live)
  (let ((frame (car (live-composer-stack live))))
    (unless (and frame (eq (lframe-kind frame) :seq))
      (error 'yaml-parse-error :message "unexpected -SEQ"))
    (pop (live-composer-stack live))))

(defun live-map-start (live anchor)
  (let ((ht (make-hash-table :test #'equal :size (live-composer-hint live))))
    (live-bind live ht anchor)
    (live-place live ht)
    (push (%lframe :map ht) (live-composer-stack live))))

(defun live-map-end (live)
  (let ((frame (car (live-composer-stack live))))
    (unless (and frame (eq (lframe-kind frame) :map))
      (error 'yaml-parse-error :message "unexpected -MAP"))
    (pop (live-composer-stack live))))

(defun live-emit (live kind anchor tag style value)
  (declare (optimize (speed 3) (safety 1)))
  (case kind
    (:stream-start)
    (:stream-end)
    (:document-start (live-doc-start live))
    (:document-end (live-doc-end live))
    (:scalar (live-scalar live value anchor tag style))
    (:alias (live-alias live value))
    (:sequence-start (live-seq-start live anchor))
    (:sequence-end (live-seq-end live))
    (:mapping-start (live-map-start live anchor))
    (:mapping-end (live-map-end live))
    (t (error 'yaml-parse-error
              :message (format nil "unexpected event ~A" kind)))))

(defun live-result (live &key all)
  (let ((docs (nreverse (live-composer-docs live))))
    (if all
        (coerce docs 'vector)
        (if docs
            (first docs)
            :null))))
