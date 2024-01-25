;;;; src/scanner-compilers.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:alexa)

;;; By default, the patterns provided are compiled into scanners via
;;; CL-PPCRE. However, CL-PPCRE trades runtime flexibility for
;;; performance. As such, we provide a way to customize how a scanner
;;; is generated and compiled.
;;;
;;; A "scanner compiler" is a function F that takes:
;;;
;;;     1. A pattern object P. The actual parsed regex tree is in
;;;     (PATTERN-PARSE-TREE P).
;;;
;;;     2. A symbol which will be bound to the string at runtime.
;;;
;;;     3. A symbol which will be bound to the starting index of the
;;;     string where scanning should begin.
;;;
;;;     4. A symbol which will be bound to an index which designates
;;;     the end of the string. (This index is exclusive.)
;;;
;;; and produces Lisp forms such that when executed, the following
;;; values will be returned:
;;;
;;;     If there is no match:
;;;
;;;         NIL is returned.
;;;
;;;     If there is a match:
;;;
;;;         1. The index of the start of the match.
;;;
;;;         2. The index (exclusive) of the end of the match.
;;;
;;;         3. A vector of starting indexes corresponding to the match
;;;         positions of each register.
;;;
;;;         4. A vector of ending indexes (exclusive) to match
;;;         positions of each register.
;;;
;;; If a scanner compiler does not accept a given pattern, then the
;;; compiler itself should error SCANNER-COMPILER-NOT-APPLICABLE.


;;; See GENERATE-PPCRE-SCANNER for the CL-PPCRE scanner compiler.
;;;
;;; See SCANNER-COMPILER-NOT-APPLICABLE.

(defun generate-string-scanner (pattern string-var start-var end-var)
  "A scanner compiler for plain string/character data."
  (let ((pattern-tree (pattern-parse-tree pattern)))
    (unless (or (stringp pattern-tree) (characterp pattern-tree))
      (error 'scanner-compiler-not-applicable :compiler 'generate-string-scanner
                                              :pattern pattern))
    (let* ((string-pattern (string pattern-tree))
           (len (length string-pattern)))
      `(cond
         ((< (- ,end-var ,start-var) ,len)
          nil)
         ((string= ,string-pattern ,string-var :start2 ,start-var :end2 (+ ,start-var ,len))
          (values ,start-var (+ ,start-var ,len) #() #()))
         (t
          nil)))))
