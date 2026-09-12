(in-package #:yaml-protocol)

;;; YAML extends JSON (CLOS). Shared mapping; extra surface is :style / decode-all.
;;; A yaml-backend is a json-backend. Do not bind *json-backend* to one by
;;; default — JSON stays on jzon (RFC 8259), not the YAML parser.

(defvar *yaml-backend* nil
  "Current YAML backend object.")

(defclass yaml-backend (json-protocol:json-backend) ()
  (:documentation "YAML 1.2 backend; subclass of json-backend."))

(defparameter *default-yaml-style* :block
  "Default encode style. :block is YAML. :json is optional JSON-schema YAML.")

(defgeneric backend-encode (backend value &key stream style)
  (:documentation "Encode VALUE as YAML. STYLE defaults to :block.
   :json is optional (JSON-schema YAML), never the default."))

(defgeneric backend-decode (backend source &key all object-class)
  (:documentation "Decode SOURCE (string, octets, or character stream).
   ALL true → vector of documents.
   OBJECT-CLASS is post-compose (see INITIALIZE-OBJECT)."))

(defun null-p (object)
  (json-protocol:null-p object))

(defun true-p (object)
  (json-protocol:true-p object))

(defun false-p (object)
  (json-protocol:false-p object))

(defun %source-string (source)
  (etypecase source
    (string source)
    ((vector (unsigned-byte 8))
     (encoding-protocol:decode source))
    (stream
     (with-output-to-string (o)
       (loop for c = (read-char source nil nil)
             while c do (write-char c o))))))

(defclass native-yaml-backend (yaml-backend) ()
  (:documentation "Built-in YAML 1.2 parser/emitter (JSON-compatible Core schema)."))

(defun make-yaml-backend ()
  (make-instance 'native-yaml-backend))

(defun initialize-object (class table)
  "Post-compose object initialization. CLASS is:
   - NIL — leave the composed value (hash-table / vector / scalar)
   - a function designator — (FUNCALL CLASS composed)
   - a class designator — MAKE-INSTANCE with keyword initargs from mapping
     keys (STRING-UPCASE interned in KEYWORD). Nested mappings stay
     hash-tables. Non-mapping root → yaml-parse-error."
  (cond
    ((null class) table)
    ((functionp class) (funcall class table))
    ((and (symbolp class) (fboundp class) (not (find-class class nil)))
     (funcall class table))
    ((not (hash-table-p table))
     (error 'yaml-parse-error
            :message "object-class requires a mapping at the document root"))
    (t
     (apply #'make-instance class
            (loop for k being the hash-keys of table using (hash-value v)
                  collect (intern (string-upcase (if (stringp k)
                                                     k
                                                     (princ-to-string k)))
                                  :keyword)
                  collect v)))))

(defun parse-events (source)
  "Parse SOURCE to a vector of YAML-EVENT (yaml-test-suite event DSL)."
  (unless *yaml-backend*
    (error 'yaml-parse-error :message "*yaml-backend* is unbound — load yaml-protocol"))
  (handler-case
      (parse-events-from-string (%source-string source))
    (yaml-error (e) (error e))
    (error (e)
      (error 'yaml-parse-error
             :message (format nil "YAML parse failed: ~A" e)))))

(defmethod backend-decode ((backend native-yaml-backend) source
                           &key all object-class)
  (declare (ignore backend))
  (let ((value
          (handler-case
              (parse-yaml (%source-string source) :all all)
            (yaml-error (e) (error e))
            (error (e)
              (error 'yaml-parse-error
                     :message (format nil "YAML parse failed: ~A" e))))))
    (cond
      ((null object-class) value)
      (all
       (map 'vector (lambda (doc) (initialize-object object-class doc)) value))
      (t (initialize-object object-class value)))))

(defun %emit (value stream emitter)
  (handler-case
      (if stream
          (progn (funcall emitter value stream) (values))
          (with-output-to-string (o)
            (funcall emitter value o)))
    (yaml-error (e) (error e))
    (error (e)
      (error 'yaml-encode-error
             :message (format nil "YAML encode failed: ~A" e)))))

(defmethod backend-encode ((backend native-yaml-backend) value &key stream style)
  (declare (ignore backend))
  (let ((style (or style *default-yaml-style*)))
    (ecase style
      (:json
       (when (graph-cyclic-p value)
         (error 'yaml-encode-error
                :message "cycle cannot be encoded as :json (no anchors)"))
       ;; Optional: reuse a loaded JSON backend. Not required.
       (if json-protocol:*json-backend*
           (json-protocol:encode value :stream stream)
           (%emit value stream #'emit-json)))
      (:block
       (%emit value stream #'emit-block)))))

(defun use-yaml-backend ()
  (setf *yaml-backend* (make-yaml-backend)))

(defun encode (value &key stream (style *default-yaml-style*))
  "Encode VALUE as YAML. Default STYLE is :block. :json is optional."
  (unless *yaml-backend*
    (error 'yaml-encode-error :message "*yaml-backend* is unbound — load yaml-protocol"))
  (backend-encode *yaml-backend* value :stream stream :style style))

(defun decode (source &key object-class)
  "Decode the first YAML 1.2 document. Valid JSON is valid YAML.
   Empty / comment-only stream → :null.
   OBJECT-CLASS is optional post-compose initialization (see INITIALIZE-OBJECT)."
  (unless *yaml-backend*
    (error 'yaml-parse-error :message "*yaml-backend* is unbound — load yaml-protocol"))
  (backend-decode *yaml-backend* source :object-class object-class))

(defun decode-all (source &key object-class)
  "Decode every YAML document in SOURCE → vector."
  (unless *yaml-backend*
    (error 'yaml-parse-error :message "*yaml-backend* is unbound — load yaml-protocol"))
  (backend-decode *yaml-backend* source :all t :object-class object-class))

(defun encode-to-octets (value &key (style *default-yaml-style*))
  (encoding-protocol:encode (encode value :style style)))

(defun decode-octets (octets &key object-class)
  (decode octets :object-class object-class))

(eval-when (:load-toplevel :execute)
  (use-yaml-backend))
