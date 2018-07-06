;;;; src/alexa.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:alexa)

(deftype non-negative-fixnum ()
  `(and fixnum unsigned-byte))

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
  short-circuit-p
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

(defun empty-match-error (regex)
  (error "Empty match on pattern ~S. This will cause infinite looping." regex))

(defmacro let-lazy (bindings &body body)
  (let* ((storage-variables (loop :for binding :in bindings
                                  :collect (gensym (symbol-name (first binding)))))
         (sentinel (gensym "SENTINEL")))
    `(let ,(loop :for var :in storage-variables :collect `(,var ',sentinel))
       (declare (ignorable ,@storage-variables))
       (symbol-macrolet ,(loop :for sv :in storage-variables
                               :for (name form) :in bindings
                               ;; If LET-LAZY were a more general
                               ;; utility outside of ALEXA, then we
                               ;; might opt to stash each FORM into a
                               ;; function, and call the function here
                               ;; to reduce code size. However, it's
                               ;; unlikely that lexer rules will
                               ;; reference these lazy variables often
                               ;; enough for that to be a concern.
                               :collect `(,name (if (eq ',sentinel ,sv)
                                                    (setf ,sv ,form)
                                                    ,sv)))
         ,@body))))

;;; Generate the code used to determine if there is a match.
(defun generate-pattern-match-code (pat execute-tag
                                    string-var start-var end-var
                                    match-start-var match-end-var
                                    reg-starts-var reg-ends-var
                                    max-match-length-var match-rule-index-var i)
  (check-type pat pattern)
  (check-type execute-tag symbol)
  (check-type string-var symbol)
  (check-type start-var symbol)
  (check-type end-var symbol)
  (check-type match-start-var symbol)
  (check-type match-end-var symbol)
  (check-type reg-starts-var symbol)
  (check-type reg-ends-var symbol)
  (check-type max-match-length-var symbol)
  (check-type match-rule-index-var symbol)
  (check-type i unsigned-byte)
  (alexandria:with-gensyms (temp-match-start
                            temp-match-end
                            temp-reg-starts
                            temp-reg-ends)
    ;; Perform the regex match.
    `(multiple-value-bind (,temp-match-start ,temp-match-end ,temp-reg-starts ,temp-reg-ends)
         (cl-ppcre:scan ',(pattern-parse-tree pat) ,string-var :start ,start-var
                                                               :end ,end-var)
       ;; Do we have a match? If not, we'll just move on to the next
       ;; thing to match.
       (if (null ,temp-match-start)
           nil
           (locally (declare (type non-negative-fixnum ,temp-match-start ,temp-match-end))
             (cond
               ;; Empty match. This could be a user bug.
               ((= ,temp-match-start ,temp-match-end)
                (empty-match-error ',(pattern-regex pat)))

               ;; We have a match, but we need to see if it's the longest
               ;; match we've seen so far.
               ((< ,max-match-length-var (- ,temp-match-end ,temp-match-start))
                ;; Record this is the best we got so far.
                (setq ,max-match-length-var (- ,temp-match-end ,temp-match-start)
                      ,match-rule-index-var ,i
                      ,match-start-var      ,temp-match-start
                      ,match-end-var        ,temp-match-end
                      ,reg-starts-var       ,temp-reg-starts
                      ,reg-ends-var         ,temp-reg-ends)
                ;; Execute immediately if we are going to short circuit.
                ,(when (pattern-short-circuit-p pat)
                   `(go ,execute-tag)))))))))

;;; Generate the code used to execute the action of a match.
(defun generate-pattern-execution-code (pat string-var
                                        match-start-var match-end-var
                                        reg-starts-var reg-ends-var)
  (check-type pat pattern)
  (check-type string-var symbol)
  (check-type match-start-var symbol)
  (check-type match-end-var symbol)
  (check-type reg-starts-var symbol)
  (check-type reg-ends-var symbol)
  (alexandria:with-gensyms (reg-start)
    (let* ((reg-vars (pattern-register-variables pat *package*))
           ($@ (intern "$@" *package*))
           ($< (intern "$<" *package*))
           ($> (intern "$>" *package*)))
      `(let ((,$< ,match-start-var)
             (,$> ,match-end-var))
         (declare (ignorable ,$< ,$>))
         ;; We lazy bind these because we don't want to
         ;; cons up the entire matched string if we
         ;; don't need to.
         (let-lazy ((,$@ (subseq ,string-var ,match-start-var ,match-end-var))
                    ,@(loop :for i :from 0
                            :for reg-var :in reg-vars
                            :collect `(,reg-var (let ((,reg-start (aref ,reg-starts-var ,i)))
                                                  (if (null ,reg-start)
                                                      nil
                                                      (subseq ,string-var
                                                              ,reg-start
                                                              (aref ,reg-ends-var ,i)))))))
           (declare (ignorable ,$@ ,@reg-vars))
           ;; Execute the pattern code.
           ,@(pattern-code pat))))))

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

The lexer will fire the action which had the longest match, and ties are broken based on the order of the actions (earlier ones are preferred). This rule can be selectively disabled for a particular action if one declares it to be a short circuiting (see below).

Signals LEXER-MATCH-ERROR as a continuable error if no match was found.

The syntax of BODY is:

    <doc string>?
    (<alias definition>*)
    <lexical action>*

An <alias definition> is a list

    (<keyword> <regex string>)

The name of the keyword may be used in the <lexical action> regexes. A <lexical action> is a list

    (<pattern spec> &body <code>)

A <pattern spec> has the following grammar:

    <pattern spec> := <regex string>
                    | (EAGER <regex string>)

The EAGER option is defined below.

The <regex string> is matched against the input string greedily and in the order they are listed in the BODY. When the longest match is found, assuming no EAGER declarations, it will execute <code>. Within <code>, the following symbols are bound:

    $1, $2, ..., $n: String match on (unnamed) register n
    $NAME          : String match on named register (?<NAME>...)
    $@             : Entire string match.
    $<, $>         : Start and end position of match.

Generally, <code> should explicitly RETURN some token object for a semantic analyzer to examine. An explicit RETURN is needed. If no RETURN is provided, then the lexer will throw away the match and move on as if the lexer were called again. (This is most often used to ignore matches, like whitespace.)

The <regex string> of the lexical action may use the names of the symbols defined in the <alias definition> forms. For example, given the alias definitions

    ((:int \"\\\\d+\")
     (:ident \"[a-z]+\"))

one can use {{INT}} and {{IDENT}} within the <regex string>s of the <lexical action>.

If the <pattern spec> uses EAGER, then the lexical action will \"short circuit\". The EAGER option states that if a match occurs on this pattern, <code> should be executed immediately, disregarding the \"longest match\" rule. This can be used for certain kinds of optimizations."
  (multiple-value-bind (definitions-and-patterns declarations doc-string)
      (alexandria:parse-body body :documentation t)
    (let* ((definitions (first definitions-and-patterns))
           (patterns (loop :for (pattern-spec . code) :in (rest definitions-and-patterns)
                           :for (regex eager)
                             := (typecase pattern-spec
                                  (string
                                   (list pattern-spec nil))
                                  (list
                                   (assert (and (= 2 (length pattern-spec))
                                                (stringp (second pattern-spec))
                                                (symbolp (first pattern-spec))
                                                (string= "EAGER" (first pattern-spec)))
                                           ()
                                           "The pattern spec ~S should either be a ~
                                             string or a list: (EAGER <string>)."
                                           pattern-spec)
                                   (list (second pattern-spec) t))
                                  (t (error "Invalid pattern spec ~S for the lexer ~
                                             definition ~S."
                                            pattern-spec name)))
                           :for parse-tree := (let ((cl-ppcre:*allow-named-registers* t))
                                                `(:SEQUENCE :START-ANCHOR
                                                            ,(cl-ppcre:parse-string
                                                              (fill-in-aliases definitions regex))))
                           :collect (multiple-value-bind (num-regs names)
                                        (extract-registers parse-tree)
                                      (make-pattern :regex regex
                                                    :short-circuit-p eager
                                                    :parse-tree parse-tree
                                                    :num-registers num-regs
                                                    :register-names names
                                                    :code code)))))
      (alexandria:with-gensyms (CONTINUE-TAG
                                EXECUTE-TAG
                                string start end
                                max-match-length
                                match-rule-index
                                match-start
                                match-end
                                reg-starts
                                reg-ends)
        `(defun ,name (,string &key ((:start ,start) 0) ((:end ,end) (length ,string)))
           ,@(alexandria:ensure-list doc-string)
           ,@declarations
           (check-type ,string simple-string)
           (check-type ,start non-negative-fixnum ":START must be a non-negative fixnum.")
           (check-type ,end non-negative-fixnum ":END must be a non-negative fixnum.")
           (assert (<= ,start ,end) (,start ,end) ":END must be not be less than :START.")
           (lambda ()
             (block nil
               ;; Our lexer state.
               (let ((,match-rule-index -1)
                     (,max-match-length 0)
                     (,match-start      0)
                     (,match-end        0)
                     (,reg-starts       #())
                     (,reg-ends         #()))
                 (declare (type fixnum ,match-rule-index)
                          (type non-negative-fixnum ,max-match-length ,match-start ,match-end)
                          (type vector ,reg-starts ,reg-ends))
                 (tagbody
                    ,CONTINUE-TAG
                    ;; If we continued, we need to have the state
                    ;; reset. We only need to reset the variables that
                    ;; determine which rules can get fired.
                    (setq ,match-rule-index -1
                          ,max-match-length 0)
                    ;; Have we finished matching string?
                    (when (= ,start ,end)
                      ;; Free STRING from closure to allow garbage
                      ;; collection.
                      (setq ,string nil)
                      ;; Return NIL indicating generator is exhausted.
                      (return nil))

                    ;; In the following pattern matching clauses, if a
                    ;; match happens, we record the longest match
                    ;; along with who matched, recorded in the
                    ;; variables MAX-MATCH-LENGTH and MATCH-RULE-INDEX
                    ;; respectively.
                    ;;
                    ;; Generate all pattern clauses.
                    ,@(loop :for i :from 0
                            :for pat :in patterns
                            :collect (generate-pattern-match-code
                                      pat EXECUTE-TAG
                                      string start end
                                      match-start match-end
                                      reg-starts reg-ends
                                      max-match-length match-rule-index i))

                    ,EXECUTE-TAG
                    (case ,match-rule-index
                      ;; Code for each of the cases.
                      ,@(loop :for i :from 0
                              :for pat :in patterns
                              :collect `(,i
                                         ;; Update our new start for
                                         ;; the next round of
                                         ;; matching.
                                         (setq ,start ,match-end)
                                         ;; Generate the code to
                                         ;; execute for this rule.
                                         ,(generate-pattern-execution-code
                                           pat
                                           string
                                           match-start match-end
                                           reg-starts reg-ends)
                                         ;; Assuming the pattern code
                                         ;; didn't exit, continue with
                                         ;; the lex loop.
                                         (go ,CONTINUE-TAG)))
                      ;; Default code if nothing found.
                      (otherwise
                       (cerror "Continue, returning NIL."
                               'lexer-match-error
                               :format-control "Couldn't find match at position ~D ~
                                                within the lexer ~S."
                               :format-arguments (list ,start ',name))
                       (return nil))))))))))))
