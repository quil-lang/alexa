;;;; src/alexa.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:alexa)

(defun walk-tree (f tree)
  (labels ((walk-tree (tree)
             (cond ((atom tree)
                    (funcall f tree))
                   (t (funcall f tree)
                      (mapc #'walk-tree tree)))))
    (walk-tree tree)
    (values)))

(defun extract-registers (parse-tree)
  (let ((num-registers 0)
        (register-names nil))
    (labels ((find-registers (x)
               (when (consp x)
                 (case (car x)
                   ((:REGISTER) (incf num-registers))
                   ((:NAMED-REGISTER) (push (cons (second x)
                                                  (incf num-registers))
                                            register-names))))))
      (walk-tree #'find-registers parse-tree)
      (values num-registers register-names))))

(defstruct pattern
  regex
  parse-tree
  num-registers
  register-names
  code)

(defun pattern-register-variables (pat package)
  (flet ((register-name (i)
           (loop :for (name . index) :in (pattern-register-names pat)
                 :when (= index i)
                   :do (return name)
                 :finally (return i))))
    (loop :for i :from 1 :to (pattern-num-registers pat)
          :collect (alexandria:format-symbol package "$~A" (register-name i)))))

(defun generate-pattern (pat continue-tag string-var start-var end-var)
  (alexandria:with-gensyms (match-start match-end
                            reg-start reg-starts reg-ends
                            pattern-block)
    `(block ,pattern-block
       (multiple-value-bind (,match-start ,match-end ,reg-starts ,reg-ends)
           (cl-ppcre:scan ',(pattern-parse-tree pat) ,STRING-VAR :start ,START-VAR
                                                                 :end ,END-VAR)
         (declare (ignorable ,reg-starts ,reg-ends))
         ;; No match
         (when (null ,match-start)
           (return-from ,pattern-block))

         ;; Empty match.
         (when (= ,match-start ,match-end)
           (error "Empty match on pattern ~S. This will cause infinite looping."
                  ',(pattern-regex pat)))

         ;; We have a match. Bind the standard symbols $@ $< $>, and
         ;; set up bindings for the register matches.
         ,(let ((reg-vars (pattern-register-variables pat *package*))
                ($@ (intern "$@" *package*))
                ($< (intern "$<" *package*))
                ($> (intern "$>" *package*)))
            `(let ((,$@ (subseq ,STRING-VAR ,MATCH-START ,MATCH-END))
                   (,$< ,START-VAR)
                   (,$> ,END-VAR)
                   ,@(loop :for i :from 0
                           :for reg-var :in reg-vars
                           :collect `(,reg-var (let ((,reg-start (aref ,reg-starts ,i)))
                                                 (if (null ,reg-start)
                                                     nil
                                                     (subseq ,STRING-VAR
                                                             ,reg-start
                                                             (aref ,reg-ends ,i)))))))
               (declare (ignorable ,$@ ,$< ,$> ,@reg-vars))
               ;; Set the new start for the next iteration.
               (setq ,START-VAR ,match-end)
               ;; Execute the pattern code.
               ,@(pattern-code pat)
               ;; Assuming the pattern code didn't exit, continue with
               ;; the lex loop.
               (go ,CONTINUE-TAG)))))))

(defun fill-in-aliases (aliases regex)
  (labels ((extract-name-from-match (match)
             (subseq match 2 (- (length match) 2)))
           (lookup-alias (alias-name)
             (let ((needle (find alias-name aliases :key #'first
                                                    :test #'string=)))
               (when (null needle)
                 (error "Undefined alias ~S in expression ~S" alias-name regex))
               (second needle))))
    (let ((alias-regex "\\{\\{(\\w|\\-|\\_)+\\}\\}")
          (resulting-regex regex))
      (cl-ppcre:do-matches-as-strings (match alias-regex regex resulting-regex)
        (let ((substitution (lookup-alias (extract-name-from-match match))))
          (setf resulting-regex (cl-ppcre:regex-replace-all `(:sequence ,match)
                                                            resulting-regex
                                                            substitution)))))))

(define-condition lexer-match-error (simple-error)
  ()
  (:documentation "Error to be signaled if the lexer didn't find a match."))

(defmacro define-string-lexer (name &body body)
  "Define a lexical analyzer named NAME.

Defining a lexical analyzer is actually defining a function named NAME whose lambda list is

    (STRING &KEY (START 0) (END (LENGTH STRING)))

The STRING is the string to be analyzed, and START/END are the starting and ending positions to be looked at. Calling the function named NAME will produce a closure which, if called repeatedly, will produce results according to the lexical rules defined. When the input string is exhausted, NIL is returned, and the string will be unbound within the closure to allow garbage collection.

Signals LEXER-MATCH-ERROR as a continuable error if no match was found.

The syntax of BODY is:

    <doc string>?
    (<alias definition>*)
    <lexical action>*

An <alias definition> is a list

    (<keyword> <regex string>)

The name of the keyword may be used in the <lexical action> regexes. A <lexical action> is a list

    (<regex string> &body <code>)

The <regex string> is matched against the input string greedily and in the order they are listed in the BODY. If a match succeeds, then it will execute <code>. Within <code>, the following symbols are bound:

    $1, $2, ..., $n: String match on (unnamed) register n
    $NAME          : String match on named register (?<NAME>...)
    $@             : Entire string match.
    $<, $>         : Start and end position of match.

Generally, <code> should explicitly RETURN some token object for a semantic analyzer to examine. An explcit RETURN is needed.

The <regex string> of the lexical action may use the names of the symbols defined in the <alias definition> forms. For example, given the alias definitions

    ((:int \"\\\\d+\")
     (:ident \"[a-z]+\"))

one can use {{INT}} and {{IDENT}} within the <regex string>s of the <lexical action>.
"
  (multiple-value-bind (definitions-and-patterns declarations doc-string)
      (alexandria:parse-body body :documentation t)
    (let* ((definitions (first definitions-and-patterns))
           (patterns (loop :for (regex . code) :in (rest definitions-and-patterns)
                           :for parse-tree := (let ((cl-ppcre:*allow-named-registers* t))
                                                `(:SEQUENCE :START-ANCHOR
                                                  ,(cl-ppcre:parse-string
                                                    (fill-in-aliases definitions regex))))
                           :collect (multiple-value-bind (num-regs names)
                                        (extract-registers parse-tree)
                                      (make-pattern :regex regex
                                                    :parse-tree parse-tree
                                                    :num-registers num-regs
                                                    :register-names names
                                                    :code code)))))
      (alexandria:with-gensyms (continue-tag string start end)
        `(defun ,name (,string &key ((:start ,start) 0) ((:end ,end) (length ,string)))
           ,@(alexandria:ensure-list doc-string)
           ,@declarations
           (check-type ,start (integer 0) ":START must be a non-negative integer.")
           (check-type ,end (integer 0) ":END must be a non-negative integer.")
           (assert (<= ,start ,end) (,start ,end) ":END must be not be less than :START.")
           (lambda ()
             (block nil
               (tagbody
                  ,continue-tag
                  ;; Have we finished matching string?
                  (when (= ,start ,end)
                    ;; Free STRING from closure to allow garbage
                    ;; collection.
                    (setq ,string nil)
                    ;; Return NIL indicating generator is exhausted.
                    (return nil))
                  ;; Generate all pattern clauses.
                  ,@(loop :for pat :in patterns
                          :collect (generate-pattern pat continue-tag string start end))
                  ;; Error clause: No match
                  (cerror "Continue, returning NIL."
                          'lexer-match-error
                         :format-control "Couldn't find match at position ~D ~
                                          within the lexer ~S."
                         :format-arguments (list ,start ',name))
                  nil))))))))
