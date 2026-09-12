(in-package #:yaml-protocol/tests)

;;; Optional: a loaded JSON backend may be reused for :style :json.
;;; These tests skip when json-backend-jzon is not present.

(defun %use-jzon ()
  (let ((pkg (find-package :json-backend-jzon)))
    (when pkg
      (funcall (intern "USE-JZON-BACKEND" pkg))
      t)))

(deftest yaml-json-subset-scalars
  (if (not (%use-jzon))
      (skip "json-backend-jzon not loaded")
      (dolist (s '("null" "true" "false" "0" "42" "-7" "1.5" "1e2" "\"hi\""))
        (ok (%lisp= (json:decode s) (decode s)) s))))

(deftest yaml-json-subset-structures
  (if (not (%use-jzon))
      (skip "json-backend-jzon not loaded")
      (dolist (s '("[]" "{}" "[1,2,3]" "{\"a\":1}"
                   "{\"a\":[1,{\"b\":null}],\"c\":false,\"d\":true}"
                   " [ 1 , { \"k\" : \"v\" } ] "))
        (ok (%lisp= (json:decode s) (decode s)) s))))

(deftest yaml-style-json-delegates-when-backend-bound
  (if (not (%use-jzon))
      (skip "json-backend-jzon not loaded")
      (let ((ht (make-hash-table :test #'equal)))
        (setf (gethash "a" ht) 1)
        (ok (string= (json:encode ht) (encode ht :style :json))))))
