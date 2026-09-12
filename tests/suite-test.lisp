(in-package #:yaml-protocol/tests)

;;; yaml-test-suite data-2022-01-17 — every in.yaml case.
;;; Events are the compliance bar. in.json checks compose (Core schema).
;;; error → must signal yaml-parse-error.

(defun %suite-root ()
  (asdf:system-relative-pathname "yaml-protocol" "tests/suite/"))

(defun %case-id (dir root)
  (string-right-trim
   "/"
   (namestring (enough-namestring dir root))))

(defun %dir-p (p)
  (and (null (pathname-name p)) (null (pathname-type p))))

(defun %suite-index-dir-p (p)
  "Skip yaml-test-suite `tags/` and `name/` indexes — they duplicate cases."
  (let ((name (car (last (pathname-directory p)))))
    (member name '("tags" "name") :test #'string=)))

(defun %collect-suite-dirs (root)
  (let ((acc '()))
    (labels ((walk (dir)
               (cond
                 ((%suite-index-dir-p dir))
                 ((probe-file (merge-pathnames "in.yaml"
                                              (uiop:ensure-directory-pathname dir)))
                  (push (uiop:ensure-directory-pathname dir) acc))
                 (t
                  (dolist (sub (uiop:subdirectories dir))
                    (walk sub))))))
      (walk root))
    (sort acc #'string< :key (lambda (p) (%case-id p root)))))

(defun %slurp (path)
  (when (probe-file path)
    (with-output-to-string (o)
      (with-open-file (in path :direction :input :element-type 'character)
        (loop for c = (read-char in nil nil)
              while c do (write-char c o))))))

(defun %normalize-nl (s)
  (if (and s (plusp (length s)) (char= (char s (1- (length s))) #\Newline))
      s
      (concatenate 'string (or s "") (string #\Newline))))

(defun %skip-json-ws (s i)
  (loop while (and (< i (length s))
                   (member (char s i) '(#\Space #\Tab #\Newline #\Return)))
        do (incf i))
  i)

(defun %parse-json-string (s i)
  (incf i)                              ; "
  (let ((chars '())
        (n (length s)))
    (loop
      (when (>= i n)
        (error "unterminated JSON string"))
      (let ((c (char s i)))
        (cond
          ((char= c #\")
           (return (values (coerce (nreverse chars) 'string) (1+ i))))
          ((char= c #\\)
           (incf i)
           (let ((e (char s i)))
             (push (case e
                     (#\" #\")
                     (#\\ #\\)
                     (#\/ #\/)
                     (#\b #\Backspace)
                     (#\f #\Page)
                     (#\n #\Newline)
                     (#\r #\Return)
                     (#\t #\Tab)
                     (#\u
                      (let ((code 0))
                        (dotimes (k 4)
                          (incf i)
                          (setf code (+ (* code 16)
                                        (digit-char-p (char s i) 16))))
                        (code-char code)))
                     (t e))
                   chars)
             (incf i)))
          (t
           (push c chars)
           (incf i)))))))

(defun %parse-json (s &optional (i 0))
  "Minimal JSON reader — same Lisp mapping as json-protocol."
  (setf i (%skip-json-ws s i))
  (when (>= i (length s))
    (error "empty JSON"))
  (let ((c (char s i)))
    (cond
      ((char= c #\")
       (%parse-json-string s i))
      ((char= c #\{)
       (incf i)
       (let ((ht (make-hash-table :test #'equal)))
         (setf i (%skip-json-ws s i))
         (when (and (< i (length s)) (char= (char s i) #\}))
           (return-from %parse-json (values ht (1+ i))))
         (loop
           (setf i (%skip-json-ws s i))
           (multiple-value-bind (k i2) (%parse-json-string s i)
             (setf i (%skip-json-ws s i2))
             (unless (char= (char s i) #\:)
               (error "expected : in JSON object"))
             (incf i)
             (multiple-value-bind (v i3) (%parse-json s i)
               (setf (gethash k ht) v
                     i (%skip-json-ws s i3))))
           (cond
             ((char= (char s i) #\})
              (return (values ht (1+ i))))
             ((char= (char s i) #\,)
              (incf i))
             (t (error "expected , or } in JSON object"))))))
      ((char= c #\[)
       (incf i)
       (setf i (%skip-json-ws s i))
       (when (and (< i (length s)) (char= (char s i) #\]))
         (return-from %parse-json (values (make-array 0) (1+ i))))
       (let ((items '()))
         (loop
           (multiple-value-bind (v i2) (%parse-json s i)
             (push v items)
             (setf i (%skip-json-ws s i2)))
           (cond
             ((char= (char s i) #\])
              (return (values (coerce (nreverse items) 'vector) (1+ i))))
             ((char= (char s i) #\,)
              (incf i)
              (setf i (%skip-json-ws s i)))
             (t (error "expected , or ] in JSON array"))))))
      ((and (<= (+ i 4) (length s)) (string= s "null" :start1 i :end1 (+ i 4)))
       (values :null (+ i 4)))
      ((and (<= (+ i 4) (length s)) (string= s "true" :start1 i :end1 (+ i 4)))
       (values t (+ i 4)))
      ((and (<= (+ i 5) (length s)) (string= s "false" :start1 i :end1 (+ i 5)))
       (values nil (+ i 5)))
      (t
       (let ((start i)
             (n (length s)))
         (when (and (< i n) (member (char s i) '(#\+ #\-)))
           (incf i))
         (loop while (and (< i n) (or (digit-char-p (char s i))
                                      (member (char s i) '(#\. #\e #\E #\+ #\-))))
               do (incf i))
         (let ((raw (subseq s start i)))
           (values (or (let ((*read-default-float-format* 'double-float)
                             (*read-eval* nil))
                         (ignore-errors
                           (let ((v (read-from-string raw)))
                             (and (numberp v) v))))
                       raw)
                   i)))))))

(defun %read-json (text)
  (multiple-value-bind (v i) (%parse-json text 0)
    (declare (ignore i))
    v))

(defparameter *suite-cases*
  (let ((root (%suite-root)))
    (mapcar (lambda (dir)
              (list (%case-id dir root)
                    dir))
            (%collect-suite-dirs root))))

(deftest yaml-test-suite
  "All yaml-test-suite data-2022-01-17 cases (events + compose + errors)."
  (ok (plusp (length *suite-cases*)) "suite corpus present")
  (dolist (row *suite-cases*)
    (destructuring-bind (id dir) row
      (testing id
        (let* ((in-yaml (%slurp (merge-pathnames "in.yaml" dir)))
               (gold (%slurp (merge-pathnames "test.event" dir)))
               (err-p (probe-file (merge-pathnames "error" dir)))
               (in-json-path (merge-pathnames "in.json" dir)))
          (if err-p
              (ok (signals (parse-events in-yaml) 'yaml-parse-error)
                  (format nil "~A should be invalid" id))
              (progn
                (let ((got (%normalize-nl (format-events (parse-events in-yaml)))))
                  (ok (string= (%normalize-nl gold) got)
                      (format nil "~A events" id)))
                (when (probe-file in-json-path)
                  (ok (%lisp= (%read-json (%slurp in-json-path))
                              (decode in-yaml))
                      (format nil "~A json" id))))))))))
