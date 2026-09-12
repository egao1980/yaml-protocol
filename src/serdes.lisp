(in-package #:yaml-protocol)

;;;; serdes-protocol implementor (:yaml).

(defclass yaml-serdes-backend (serdes-protocol:serdes-backend) ()
  (:documentation "serdes backend that delegates to *YAML-BACKEND*."))

(defun make-yaml-serdes-backend ()
  (make-instance 'yaml-serdes-backend))

(defmethod serdes-protocol:backend-encode ((backend yaml-serdes-backend) value
                                           &key stream (style *default-yaml-style*))
  (declare (ignore backend))
  (encode value :stream stream :style style))

(defmethod serdes-protocol:backend-decode ((backend yaml-serdes-backend) source
                                           &key object-class)
  (declare (ignore backend))
  (decode source :object-class object-class))

(defclass yaml-character-input-stream (serdes-protocol:serdes-character-input-stream)
  ((buffer :initform nil :accessor yaml-stream-buffer)
   (docs :initform nil :accessor yaml-stream-docs)
   (index :initform 0 :accessor yaml-stream-index)))

(defclass yaml-character-output-stream (serdes-protocol:serdes-character-output-stream)
  ((first-p :initform t :accessor yaml-stream-first-p)))

(defmethod serdes-protocol:backend-make-input-stream ((backend yaml-serdes-backend)
                                                      underlying
                                                      &key (element-type 'character))
  (unless (subtypep element-type 'character)
    (error 'yaml-error :message "yaml streams are character"))
  (make-instance 'yaml-character-input-stream
                 :underlying underlying
                 :backend backend))

(defmethod serdes-protocol:backend-make-output-stream ((backend yaml-serdes-backend)
                                                       underlying
                                                       &key (element-type 'character))
  (unless (subtypep element-type 'character)
    (error 'yaml-error :message "yaml streams are character"))
  (make-instance 'yaml-character-output-stream
                 :underlying underlying
                 :backend backend))

(defun %slurp-char-stream (s)
  (with-output-to-string (o)
    (loop for c = (read-char s nil nil)
          while c do (write-char c o))))

(defun %ensure-docs (stream)
  (unless (yaml-stream-docs stream)
    (let ((text (%slurp-char-stream (serdes-protocol:underlying-stream stream))))
      (setf (yaml-stream-docs stream) (decode-all text)
            (yaml-stream-index stream) 0))))

(defmethod serdes-protocol:stream-decode-value ((stream yaml-character-input-stream) &key)
  (%ensure-docs stream)
  (let ((docs (yaml-stream-docs stream))
        (i (yaml-stream-index stream)))
    (if (>= i (length docs))
        :eof
        (prog1 (aref docs i)
          (incf (yaml-stream-index stream))))))

(defmethod serdes-protocol:stream-encode-value ((stream yaml-character-output-stream) value &key)
  (let ((out (serdes-protocol:underlying-stream stream)))
    (unless (yaml-stream-first-p stream)
      (write-string "---" out)
      (write-char #\Newline out))
    (setf (yaml-stream-first-p stream) nil)
    (encode value :stream out :style *default-yaml-style*)
    (write-char #\Newline out)
    value))

(defun use-yaml-serdes-backend ()
  "Register and select the YAML serdes backend. Returns the backend."
  (let ((backend (make-yaml-serdes-backend)))
    (serdes-protocol:register-format :yaml backend)
    (setf serdes-protocol:*serdes-format* :yaml
          serdes-protocol:*serdes-backend* backend)
    backend))

(eval-when (:load-toplevel :execute)
  (use-yaml-serdes-backend))
