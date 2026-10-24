(defsystem "yaml-protocol"
  :version "0.2.1"
  :description "CLOS YAML 1.2 event parser/composer; extends json-protocol (JSON ⊂ YAML); implements serdes-protocol :yaml"
  :author "egao1980"
  :license "MIT"
  :depends-on ("encoding-protocol" "json-protocol" "serdes-protocol")
  ;; Own GHCR repo. Without this, discover can put json-backend-jzon first and
  ;; the owning-repo token 403s pushing that package.
  :properties (:cl-repo (:provides ("yaml-protocol")
                         :ci (:with ("json-backend-jzon"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "events")
               (:file "parser")
               (:file "emitter")
               (:file "protocol")
               (:file "serdes"))
  :in-order-to ((test-op (test-op "yaml-protocol/tests"))))

(defsystem "yaml-protocol/tests"
  :depends-on ("yaml-protocol" "json-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "yaml-test")
               (:file "json-interop-test")
               (:file "suite-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
