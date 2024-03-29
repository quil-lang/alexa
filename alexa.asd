;;;; alexa.asd
;;;;
;;;; Author: Robert Smith
;;;;
;;;; Copyright (c) 2016-2018 Rigetti & Co, Inc.
;;;;
;;;; Copyright (c) 2018-2024 Alexa Contributors

(asdf:defsystem #:alexa
  :description "A lexical analyzer generator."
  :author "Robert Smith <robert@stylewarning.com>"
  :license "BSD 3-clause (See LICENSE.txt)"
  :version (:read-file-form "VERSION.txt")
  :depends-on (#:alexandria
               #:cl-ppcre)
  :in-order-to ((asdf:test-op (asdf:test-op #:alexa-tests)))
  :serial t
  :components ((:static-file "LICENSE.txt")
               (:module src
                :serial t
                :components ((:file "package")
                             (:file "alexa")
                             (:file "scanner-compilers")))))

