(defpackage #:yaml-protocol
  (:use #:cl)
  (:nicknames #:stack-yaml)
  (:export #:yaml-error
           #:yaml-parse-error
           #:yaml-encode-error
           #:yaml-unsupported-feature
           #:yaml-error-message

           #:*yaml-backend*
           #:*default-yaml-style*
           #:yaml-backend
           #:backend-encode
           #:backend-decode
           #:make-yaml-backend
           #:use-yaml-backend

           #:encode
           #:decode
           #:decode-all
           #:encode-to-octets
           #:decode-octets

           #:null-p
           #:true-p
           #:false-p

           #:yaml-serdes-backend
           #:make-yaml-serdes-backend
           #:use-yaml-serdes-backend))

(in-package #:yaml-protocol)
