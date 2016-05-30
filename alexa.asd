;;;; alexa.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:alexa
  :description "A lexical analyzer generator"
  :author "Robert Smith <robert@rigetti.com>"
  :pathname "src/"
  :depends-on (#:alexandria
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "alexa")))

