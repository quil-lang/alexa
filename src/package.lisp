;;;; package.lisp
;;;;
;;;; Author: Robert Smith

(defpackage #:alexa
  (:documentation "A lexical analyzer generator.")
  (:use #:cl)
  (:export
   #:define-string-lexer                ; MACRO
   #:lexer-match-error                  ; CONDITION
   )
  )
