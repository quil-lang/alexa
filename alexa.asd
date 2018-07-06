;;;; alexa.asd
;;;;
;;;; Author: Robert Smith
;;;;
;;;; Copyright (c) 2016-2018 Rigetti & Co, Inc.

(asdf:defsystem #:alexa
  :description "A lexical analyzer generator."
  :author "Robert Smith <robert@rigetti.com>"
  :license "BSD 3-clause (See LICENSE.txt)"
  :version (:read-file-form "VERSION.txt")
  :depends-on (#:alexandria
               #:cl-ppcre)
  :serial t
  :components ((:static-file "LICENSE.txt")
               (:module src
                :serial t
                :components ((:file "package")
                             (:file "alexa")))))

