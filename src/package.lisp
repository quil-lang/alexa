;;;; package.lisp
;;;;
;;;; Author: Robert Smith

(defpackage #:alexa
  (:documentation "A lexical analyzer generator.")
  (:use #:cl)
  (:export
   #:define-string-lexer                ; MACRO
   #:lexer-match-error                  ; CONDITION
   #:lexer-match-error-lexer-name       ; READER
   #:lexer-match-error-position         ; READER
   #:no-match-error                     ; CONDITION
   #:empty-match-error                  ; CONDITION
   #:empty-match-error-pattern          ; READER
   )
  )

(defpackage #:alexa-internal
  (:documentation "A package to stash away generated symbols."))
