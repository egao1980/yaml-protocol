(in-package #:yaml-protocol/tests)

(defun %lisp= (a b &optional (seen (make-hash-table :test #'eq)))
  (cond
    ((eq a b) t)
    ((and (hash-table-p a) (hash-table-p b))
     (let ((pair (cons a b)))
       (or (gethash pair seen)
           (progn
             (setf (gethash a seen) b)
             (and (= (hash-table-count a) (hash-table-count b))
                  (loop for k being the hash-keys of a using (hash-value v)
                        always (and (nth-value 1 (gethash k b))
                                    (%lisp= v (gethash k b) seen))))))))
    ((and (vectorp a) (not (stringp a))
          (vectorp b) (not (stringp b)))
     (or (eq a b)
         (and (= (length a) (length b))
              (loop for i from 0 below (length a)
                    always (%lisp= (aref a i) (aref b i) seen)))))
    ((and (floatp a) (floatp b))
     (< (abs (- a b)) 1d-9))
    ((and (numberp a) (numberp b))
     (= a b))
    (t (equal a b))))

(deftest yaml-json-style-without-json-backend
  "Native :style :json — no json-backend required."
  (let ((json:*json-backend* nil)
        (ht (make-hash-table :test #'equal)))
    (setf (gethash "a" ht) 1
          (gethash "z" ht) :null
          (gethash "ok" ht) t
          (gethash "no" ht) nil)
    (let ((text (encode ht :style :json)))
      (ok (string= "{\"a\":1,\"no\":false,\"ok\":true,\"z\":null}" text))
      (ok (%lisp= ht (decode text))))))

(deftest yaml-block-roundtrip
  (let ((ht (decode (format nil "a: 1~%b: null~%c:~%  - x~%  - y~%"))))
    (ok (hash-table-p ht))
    (ok (= 1 (gethash "a" ht)))
    (ok (eq :null (gethash "b" ht)))
    (ok (equalp #("x" "y") (gethash "c" ht)))
    (ok (%lisp= ht (decode (encode ht :style :block))))))

(deftest yaml-comments
  (let ((v (decode (format nil "# head~%foo: bar # tail~%"))))
    (ok (string= "bar" (gethash "foo" v)))))

(deftest yaml-norway-is-string
  "YAML 1.2 Core — NO is not a boolean (unlike YAML 1.1)."
  (ok (string= "NO" (decode "NO")))
  (ok (string= "Yes" (decode "Yes"))))

(deftest yaml-nonspecific-tag-is-string
  "[100] c-non-specific-tag `!` resolves to str (S4JQ)."
  (ok (equalp #("12" 12 "12")
              (decode (format nil "- \"12\"~%- 12~%- ! 12~%")))))

(deftest yaml-core-bools
  (ok (eq t (decode "true")))
  (ok (eq nil (decode "false")))
  (ok (eq :null (decode "~")))
  (ok (eq :null (decode ""))))

(deftest yaml-hex-oct
  (ok (= 255 (decode "0xff")))
  (ok (= 8 (decode "0o10"))))

(deftest yaml-quoted
  (ok (string= "a:b" (decode "\"a:b\"")))
  (ok (string= "it's" (decode "'it''s'")))
  (ok (string= (format nil "a~%b") (decode "\"a\\nb\""))))

(deftest yaml-literal-block
  (ok (string= (format nil "hi~%there~%")
               (decode (format nil "|~%  hi~%  there~%")))))

(deftest yaml-nested-block
  (let ((v (decode (format nil "outer:~%  inner: 2~%"))))
    (ok (= 2 (gethash "inner" (gethash "outer" v))))))

(deftest yaml-anchors
  (let ((v (decode (format nil "a: &x 1~%b: *x~%"))))
    (ok (= 1 (gethash "a" v)))
    (ok (= 1 (gethash "b" v)))))

(deftest yaml-multi-doc
  (let ((docs (decode-all (format nil "1~%---~%2~%"))))
    (ok (equalp #(1 2) docs))))

(deftest yaml-serdes
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "k" ht) "v")
    (ok (%lisp= ht (serdes-protocol:decode
                    (serdes-protocol:encode ht :format :yaml)
                    :format :yaml)))))

(deftest yaml-jsonl-style-stream
  (let ((raw (with-output-to-string (o)
               (let ((out (serdes-protocol:make-output-stream o :format :yaml)))
                 (serdes-protocol:stream-encode-value out 1)
                 (serdes-protocol:stream-encode-value out 2))))
        (acc '()))
    (let ((in (serdes-protocol:make-input-stream
               (make-string-input-stream raw) :format :yaml)))
      (loop
        (let ((v (serdes-protocol:stream-decode-value in)))
          (when (eq v :eof) (return))
          (push v acc))))
    (ok (equal '(2 1) acc))))

(deftest yaml-predicates
  (ok (null-p :null))
  (ok (true-p t))
  (ok (false-p nil))
  (ok (not (false-p :null))))

(deftest encode-default-is-block
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "a" ht) 1
          (gethash "b" ht) 2)
    (let ((s (encode ht)))
      (ok (not (eql (char s 0) #\{)))
      (ok (search "a: " s))
      (ok (eq :block *default-yaml-style*)))))

(deftest yaml-merge-alias
  (let ((v (decode (format nil "base: &b~%  a: 1~%  b: 2~%item:~%  <<: *b~%  c: 3~%"))))
    (let ((item (gethash "item" v)))
      (ok (= 1 (gethash "a" item)))
      (ok (= 2 (gethash "b" item)))
      (ok (= 3 (gethash "c" item))))))

(defclass %yaml-person ()
  ((name :initarg :name :reader %person-name)
   (age :initarg :age :reader %person-age)))

(deftest yaml-optional-object-init
  (let ((p (decode (format nil "name: Ada~%age: 36~%")
                   :object-class '%yaml-person)))
    (ok (typep p '%yaml-person))
    (ok (string= "Ada" (%person-name p)))
    (ok (= 36 (%person-age p))))
  (let ((ht (decode (format nil "name: Ada~%"))))
    (ok (hash-table-p ht))))

(deftest yaml-alias-is-eq
  "Compose shares the object. Cycles are allowed."
  (let ((v (decode (format nil "a: &x [1, 2]~%b: *x~%"))))
    (ok (eq (gethash "a" v) (gethash "b" v))))
  (let ((cyc (decode (format nil "&s~%- 1~%- *s~%"))))
    (ok (vectorp cyc))
    (ok (= 1 (aref cyc 0)))
    (ok (eq cyc (aref cyc 1))))
  (let ((m (decode (format nil "&m~%self: *m~%n: 1~%"))))
    (ok (eq m (gethash "self" m)))
    (ok (= 1 (gethash "n" m)))))

(deftest yaml-encode-cycle-uses-visited-ids
  (let ((m (make-hash-table :test #'equal)))
    (setf (gethash "self" m) m
          (gethash "n" m) 1)
    (ok (graph-cyclic-p m))
    (let ((text (encode m :style :block)))
      (ok (search "&id" text))
      (ok (search "*id" text))
      (let ((round (decode text)))
        (ok (eq round (gethash "self" round)))
        (ok (= 1 (gethash "n" round)))))
    (ok (signals (encode m :style :json) 'yaml-encode-error)))
  (let ((v (make-array 2 :adjustable t :fill-pointer 2)))
    (setf (aref v 0) 1
          (aref v 1) v)
    (let ((text (encode v :style :block)))
      (ok (search "&id" text))
      (ok (search "*id" text))
      (let ((round (decode text)))
        (ok (eq round (aref round 1)))))))

(deftest yaml-events-smoke
  (let ((ev (parse-events (format nil "a: &x 1~%b: *x~%"))))
    (ok (vectorp ev))
    (ok (equal '(:stream-start :document-start :mapping-start
                 :scalar :scalar :scalar :alias
                 :mapping-end :document-end :stream-end)
               (map 'list #'yaml-event-kind ev)))
    (ok (string= "x" (yaml-event-anchor (elt ev 4))))
    (ok (string= "x" (yaml-event-value (elt ev 6))))
    (ok (search "=ALI *x" (format-events ev)))))

(deftest yaml-json-fast-path-and-fallback
  "Strict JSON hits the fast path. YAML-only / leftover → full parser."
  (ok (= 1 (gethash "a" (decode "{\"a\":1}"))))
  (ok (= 1 (gethash "a" (decode "{a:1}"))))
  (ok (= 1 (gethash "a" (decode (format nil "{\"a\":1} # c")))))
  (ok (eq nil (decode "false")))
  (ok (eq :null (decode "null")))
  (ok (eq :null (decode "")))
  (ok (equalp #() (decode-all "")))
  (ok (equalp #(1) (decode-all "1")))
  (ok (equalp #(1 2) (decode-all (format nil "1~%---~%2~%")))))

(deftest yaml-extends-json
  "YAML is a CLOS extension of JSON, not a sibling and not the parent."
  (ok (subtypep 'yaml-backend 'json:json-backend))
  (ok (subtypep 'yaml-error 'json:json-error))
  (ok (subtypep 'yaml-parse-error 'json:json-parse-error))
  (ok (subtypep 'yaml-encode-error 'json:json-encode-error))
  (ok (signals (decode "[") 'json:json-parse-error)))
