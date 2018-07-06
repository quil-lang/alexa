;;;; alexa-tests.asd
;;;;
;;;; Author: Robert Smith
;;;;
;;;; Copyright (c) 2018 Rigetti & Co, Inc.

(asdf:defsystem #:alexa-tests
  :description "Regression tests for ALEXA."
  :author "Robert Smith <robert@rigetti.com>"
  :license "BSD 3-clause (See LICENSE.txt)"
  :depends-on (#:alexa
               #:fiasco
               #:uiop)
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call ':alexa-tests
                                           '#:run-alexa-tests))
  :serial t
  :components ((:module tests
                :serial t
                :components ((:file "tests")))))
