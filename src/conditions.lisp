(in-package #:yaml-protocol)

;;; YAML extends JSON: handlers of json-error see YAML failures.
;;; Do not invert — json-protocol must not depend on this package.

(define-condition yaml-error (json-protocol:json-error) ()
  (:report (lambda (c s)
             (format s "YAML error~@[: ~a~]" (json-protocol:json-error-message c)))))

(define-condition yaml-parse-error (yaml-error json-protocol:json-parse-error) ())
(define-condition yaml-encode-error (yaml-error json-protocol:json-encode-error) ())
(define-condition yaml-unsupported-feature (yaml-error) ())

(defun yaml-error-message (condition)
  "Synonym for json-error-message (shared slot)."
  (json-protocol:json-error-message condition))
