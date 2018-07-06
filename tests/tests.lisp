;;;; tests.lisp
;;;;
;;;; Author: Robert Smith

(fiasco:define-test-package #:alexa-tests
    (:use #:alexa)

  ;; suite.lisp
  (:export
   #:run-alexa-tests))

(in-package #:alexa-tests)

(defun run-alexa-tests (&key (verbose nil) (headless nil))
  "Run all ALEXA tests. If VERBOSE is T, print out lots of test info. If HEADLESS is T, disable interactive debugging and quit on completion."
  ;; Bug in Fiasco commit fe89c0e924c22c667cc11c6fc6e79419fc7c1a8b
  (setf fiasco::*test-run-standard-output* (make-broadcast-stream
                                            *standard-output*))
  (cond
    ((null headless)
     (run-package-tests :package ':alexa-tests
                        :verbose verbose
                        :describe-failures t
                        :interactive t))
    (t
     (let ((successp (run-package-tests :package ':alexa-tests
                                        :verbose t
                                        :describe-failures t
                                        :interactive nil)))
       (uiop:quit (if successp 0 1))))))

(defun lex-count (lex-closure)
  (loop :with ntoks := 0
        :for tok := (funcall lex-closure)
        :while tok
        :do (incf ntoks)
        :finally (return ntoks)))

(defun lex (lexer string)
  (loop :with lex-closure := (funcall lexer string)
        :for tok := (funcall lex-closure)
        :while tok
        :collect tok))

(defun is-lex (lexer string expected-result)
  (is (equal expected-result (lex lexer string))))

(define-string-lexer simple-lexer
  ()
  ("\\s+" nil)
  ("a" (return #\a))
  ("b" (return #\b)))

(deftest test-simple-lexer ()
  "Test basic lexing functionality."
  (is-lex #'simple-lexer "" '())
  (is-lex #'simple-lexer "   " '())
  (is-lex #'simple-lexer "a" '(#\a))
  (is-lex #'simple-lexer "b" '(#\b))
  (is-lex #'simple-lexer "ab" '(#\a #\b))
  (is-lex #'simple-lexer " ab aaa bbb " '(#\a #\b #\a #\a #\a #\b #\b #\b)))

(define-string-lexer alias-lexer
  ((:num "\\d+"))
  ("{{NUM}}" (return (parse-integer $@))))

(deftest test-alias-lexer ()
  "Test that the alias functionality seems to work."
  (dotimes (i 101)
    (is-lex #'alias-lexer (format nil "~D" i) (list i))))

(define-string-lexer position-lexer
  ()
  ("\\s+" nil)
  ("a+" (return (cons $< $>))))

(deftest test-position-lexer ()
  "Test that positions $< and $> are calculated correctly."
  (is-lex #'position-lexer "a" '((0 . 1)))
  (is-lex #'position-lexer " a" '((1 . 2)))
  (is-lex #'position-lexer " a a a" '((1 . 2) (3 . 4) (5 . 6)))
  (is-lex #'position-lexer "a aa aaa" '((0 . 1) (2 . 4) (5 . 8))))

(alexa:define-string-lexer arith-lexer
  ((:num "\\d+")
   (:name "[A-Za-z][A-Za-z0-9_]*"))
  ("pi"       (return pi))
  ("{{NAME}}" (return $@))
  ("{{NUM}}"  (return (parse-integer $@)))
  ("[+*/-]"   (return (intern $@ :keyword)))
  ("\\("      (return #\())
  ("\\)"      (return #\)))
  ("\\s+"     nil))

(alexa:define-string-lexer arith-lexer-opt
  ((:num "\\d+")
   (:name "[A-Za-z][A-Za-z0-9_]*"))
  ((eager "{{NUM}}")  (return (parse-integer $@)))
  ((eager "[+*/-]")   (return (intern $@ :keyword)))
  ((eager "\\(")      (return #\())
  ((eager "\\)")      (return #\)))
  ((eager "\\s+")     nil)
  ("pi"               (return pi))
  ((eager "{{NAME}}") (return $@)))

(deftest test-arith-lexers ()
  "Test that a more complicated lexer works, including EAGER."
  (flet ((test (string result)
           (is (is-lex #'arith-lexer string result))
           (is (is-lex #'arith-lexer-opt string result))))
    (test "a" '("a"))
    (test "pi" (list pi))
    (test "123" '(123))
    (test "(  )" '(#\( #\)))
    (test "+ - * /" '(:+ :- :* :/))
    (test "a123 a 123" '("a123" "a" 123))
    (test "pi pip pine pi_2" (list pi "pip" "pine" "pi_2"))))
