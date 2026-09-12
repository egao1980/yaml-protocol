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

           #:yaml-event
           #:yaml-event-p
           #:yaml-event-kind
           #:yaml-event-implicit
           #:yaml-event-flow-p
           #:yaml-event-anchor
           #:yaml-event-tag
           #:yaml-event-style
           #:yaml-event-value
           #:yaml-events
           #:yaml-events-p
           #:yaml-events-count
           #:event-kind
           #:event-implicit
           #:event-flow-p
           #:event-anchor
           #:event-tag
           #:event-style
           #:event-value
           #:box-events
           #:parse-events
           #:format-events
           #:compose-events
           #:graph-cyclic-p

           #:yaml-serdes-backend
           #:make-yaml-serdes-backend
           #:use-yaml-serdes-backend))

(in-package #:yaml-protocol)
