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
  ;; The regex string.
  (regex nil :type string)
  ;; Whether this pattern short-circuits.
  (short-circuit-p nil :type boolean)
  ;; The regex tree.
  parse-tree
  ;; A compiler for the parse tree.
  ;;
  ;; See "Scanner Compilers" section below for details.
  (compiler 'generate-ppcre-scanner :type symbol)
  ;; The number of registers.
  (num-registers nil :type integer)
  ;; The name of the registers.
  (register-names nil :type list)
  ;; Name of the function that scans this regex.
  (scanner-name nil :type symbol)
  ;; Name of the function that fires this rule.
  (fire-name nil :type symbol)
  ;; The raw code forms that fire.
  code)

(defun pattern-register-variables (pat package)
  (flet ((register-name (i)
           (loop :for (name . index) :in (pattern-register-names pat)
                 :when (= index i)
                   :do (return name)
                 :finally (return i))))
    (loop :for i :from 1 :to (pattern-num-registers pat)
          :collect (alexandria:format-symbol package "$~A" (register-name i)))))

(define-condition lexer-match-error (error)
  ((lexer-name :initarg :lexer-name
               :reader lexer-match-error-lexer-name
               :type symbol
               :documentation "The name of the lexer from which this condition came.")
   (position :initarg :position
             :reader lexer-match-error-position
             :type unsigned-byte
             :documentation "The position in the string being matched where the failure occured."))
  (:documentation "Superclass of all possible runtime errors regarding matches during lexing."))

(define-condition no-match-error (lexer-match-error)
  ()
  (:report (lambda (c s)
             (format s "Couldn't find match at position ~D within the lexer ~S."
                     (lexer-match-error-position c)
                     (lexer-match-error-lexer-name c))))
  (:documentation "Error to be signaled if the lexer didn't find a match."))

(define-condition empty-match-error (lexer-match-error)
  ((pattern :initarg :pattern
            :reader empty-match-error-pattern))
  (:report (lambda (c s)
             (format s "Within the lexer ~S, got an empty match on pattern ~S at position ~
                        ~D. This will cause infinite looping."
                     (lexer-match-error-lexer-name c)
                     (empty-match-error-pattern c)
                     (lexer-match-error-position c))))
  (:documentation "Error that is signaled when an empty string was matched."))

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
(defun generate-pattern-match-code (name pat execute-tag
                                    string-var start-var end-var
                                    match-start-var match-end-var
                                    reg-starts-var reg-ends-var
                                    max-match-length-var match-rule-index-var i)
  (check-type name symbol)
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
         (,(pattern-scanner-name pat) ,string-var ,start-var ,end-var)
       ;; Do we have a match? If not, we'll just move on to the next
       ;; thing to match.
       (if (null ,temp-match-start)
           nil
           (locally (declare (type non-negative-fixnum ,temp-match-start ,temp-match-end))
             (cond
               ;; Empty match. This could be a user bug.
               ((= ,temp-match-start ,temp-match-end)
                (cerror "Continue, returning NIL."
                        'empty-match-error
                        :pattern ',(pattern-regex pat)
                        :lexer-name ',name
                        :position ,temp-match-start)
                (return nil))

               ;; We have a match, but we need to see if it's the
               ;; longest match we've seen so far.
               ((< ,max-match-length-var (- ,temp-match-end ,temp-match-start))
                ;; Record this is the best we got so far.
                (setq ,max-match-length-var (- ,temp-match-end ,temp-match-start)
                      ,match-rule-index-var ,i
                      ,match-start-var      ,temp-match-start
                      ,match-end-var        ,temp-match-end
                      ,reg-starts-var       ,temp-reg-starts
                      ,reg-ends-var         ,temp-reg-ends)
                ;; Execute immediately if we are going to short
                ;; circuit.
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
         (declare (type non-negative-fixnum ,$< ,$>)
                  (ignorable ,$< ,$>))
         ;; We lazy bind these because we don't want to cons up the
         ;; entire matched string if we don't need to.
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


;;; Scanner Compilers
;;;
;;; See "scanner-compilers.lisp" for a description of scanner
;;; compilers and additional implementations.

(define-condition scanner-compiler-not-applicable (serious-condition)
  ((pattern :initarg :pattern :reader scanner-compiler-not-applicable-pattern)
   (compiler :initarg :compiler :reader scanner-compiler-not-applicable-compiler)))

(defun generate-ppcre-scanner (pattern string-var start-var end-var)
  (alexandria:with-gensyms (scanner)
    `(let ((,scanner (load-time-value
                      ;; Introduced in CL-PPCRE
                      ;; 2.1.0. We'll keep it
                      ;; double-coloned and IGNORABLE for
                      ;; backwards compatibility... for
                      ;; now...
                      (let ((cl-ppcre::*look-ahead-for-suffix* nil))
                        (declare (ignorable cl-ppcre:*look-ahead-for-suffix*))
                        (cl-ppcre:create-scanner ',`(:SEQUENCE :START-ANCHOR ,(pattern-parse-tree pattern))
                                                 :single-line-mode t))
                      t)))
       (cl-ppcre:scan ,scanner ,string-var :start ,start-var :end ,end-var))))


;;; Execution Dispatch
;;;
;;; This refers to how we invoke a "fire" function once we've
;;; determined which scanner succeeded.

(deftype execution-dispatch-mode ()
  `(member :function-vector :case))

(defun generate-function-vector-dispatch-code (match-rule-index
                                               patterns
                                               string
                                               match-start
                                               match-end
                                               reg-starts
                                               reg-ends)
  `(return
     (funcall
      (the function
           (aref (load-time-value
                  (vector
                   ,@(loop
                       :for pat :in patterns
                       :collect
                       `(function ,(pattern-fire-name pat))))
                  t)
                 ,match-rule-index))
      ,string
      ,match-start
      ,match-end
      ,reg-starts
      ,reg-ends)))

(defun generate-case-dispatch-code (match-rule-index
                                    patterns
                                    string
                                    match-start
                                    match-end
                                    reg-starts
                                    reg-ends)
  `(return
     (ecase ,match-rule-index
       ,@(loop :for i :from 0
               :for pat in patterns
               :collect `((,i)          ; Case #
                          ;; Result
                          (,(pattern-fire-name pat)
                           ,string
                           ,match-start
                           ,match-end
                           ,reg-starts
                           ,reg-ends))))))


;;; DEFINE-LEXER Implementation

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

(defun parse-pattern-spec (spec)
  "Parse a \"pattern spec\", the places of DEFINE-LEXER where regex patterns are specified. The current supported syntax is:

    <spec> := <regex>
            | (<regex> <option>*)
            | (EAGER <regex> <option>*)

    <option> := :COMPILER <symbol>"
  (typecase spec
    (string
     (values spec nil))
    (cons
     (cond
       ((stringp (car spec))
        (values (car spec) (cdr spec)))
       ((and (symbolp (car spec))
             (string= "EAGER" (car spec))
             (not (null (cdr spec)))
             (stringp (cadr spec)))
        (values (cadr spec) (list* ':eager 't (cddr spec))))))
    (t
     (error "Invalid pattern: ~S" spec))))

;;; These options control some aspects of code generation.

(defvar *execution-dispatch-mode*)
(defvar *inline-match-scanners*)
(defvar *inline-match-fires*)

(defmacro define-string-lexer (name &body body)
  "Define a lexical analyzer named NAME.

Defining a lexical analyzer is actually defining a function named NAME whose lambda list is

    (STRING &KEY (START 0) (END (LENGTH STRING)))

The STRING is the string to be analyzed, and START/END are the starting and ending positions to be looked at. Calling the function named NAME will produce a closure which, if called repeatedly, will produce results according to the lexical rules defined. When the input string is exhausted, NIL is returned, and the string will be unbound within the closure to allow garbage collection.

If STRING is not a SIMPLE-STRING, then it will be coerced into one (which will cons).

The lexer will fire the action which had the longest match, and ties are broken based on the order of the actions (earlier ones are preferred). This rule can be selectively disabled for a particular action if one declares it to be a short circuiting (see below).

Signals a continuable error LEXER-MATCH-ERROR in the event of one of the following:

    - NO-MATCH-ERROR is signaled if no match was found.

    - EMPTY-MATCH-ERROR is signaled if a match would result in an infinite loop, because an empty string was matched.

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

Generally, <code> should explicitly RETURN some token object for a semantic analyzer to examine. Currently, only a single value can be returned. (All other values will be ignored.) An explicit RETURN is needed. If no RETURN is provided, then the lexer will throw away the match and move on as if the lexer were called again. (This is most often used to ignore matches, like whitespace.)

The <regex string> of the lexical action may use the names of the symbols defined in the <alias definition> forms. For example, given the alias definitions

    ((:int \"\\\\d+\")
     (:ident \"[a-z]+\"))

one can use {{INT}} and {{IDENT}} within the <regex string>s of the <lexical action>.

If the <pattern spec> uses EAGER, then the lexical action will \"short circuit\". The EAGER option states that if a match occurs on this pattern, <code> should be executed immediately, disregarding the \"longest match\" rule. This can be used for certain kinds of optimizations."
  (destructuring-bind (name &key ((:inline-scanners inline-match-scanners)
                                  (if (boundp '*inline-match-scanners*)
                                      *inline-match-scanners*
                                      nil))
                                 ((:inline-fires inline-match-fires)
                                  (if (boundp '*inline-match-fires*)
                                      *inline-match-fires*
                                      nil))
                                 ((:dispatch-mode dispatch-mode)
                                  (if (boundp '*execution-dispatch-mode*)
                                      *execution-dispatch-mode*
                                      ':function-vector)))
      (alexandria:ensure-list name)
    (check-type name symbol)
    (check-type dispatch-mode execution-dispatch-mode)
    (multiple-value-bind (definitions-and-patterns declarations doc-string)
        (alexandria:parse-body body :documentation t)
      (let* ((definitions (first definitions-and-patterns))
             (patterns (loop :for i :from 0
                             :for (pattern-spec . code) :in (rest definitions-and-patterns)
                             :for (regex options)
                               := (multiple-value-list (parse-pattern-spec pattern-spec))
                             :for parse-tree := (let ((cl-ppcre:*allow-named-registers* t))
                                                  (cl-ppcre:parse-string
                                                   (fill-in-aliases definitions regex)))
                             :collect
                             (multiple-value-bind (num-regs names)
                                 (extract-registers parse-tree)
                               (let ((*package* (find-package :cl))) ; print symbols in full
                                 (make-pattern :regex regex
                                               :short-circuit-p (getf options ':eager)
                                               :compiler (getf options ':compiler 'generate-ppcre-scanner)
                                               :parse-tree parse-tree
                                               :num-registers num-regs
                                               :register-names names
                                               ;; We generate these
                                               ;; symbols in this
                                               ;; package so they're
                                               ;; overwritten upon
                                               ;; redefinition.
                                               :scanner-name
                                               (alexandria:format-symbol
                                                ':alexa-internal
                                                "~S/SCANNER-~D" name i)
                                               :fire-name
                                               (alexandria:format-symbol
                                                ':alexa-internal
                                                "~S/FIRE-~D" name i)
                                               :code code))))))
        (alexandria:with-gensyms (CONTINUE-TAG
                                  EXECUTE-TAG
                                  string start end
                                  max-match-length
                                  match-rule-index
                                  match-start
                                  match-end
                                  reg-starts
                                  reg-ends
                                  sentinel)
          `(progn
             ;; Generate the functions that scan and fire the rules. We
             ;; generate DEFUNs here to make it easier to instrument the
             ;; rules.
             ,@(loop
                 :for rule-number :from 0
                 :for pat :in patterns
                 :for scanner-name := (pattern-scanner-name pat)
                 :for fire-name := (pattern-fire-name pat)

                 ;; Generate the SCANNER function.
                 :collect `(declaim (inline ,scanner-name))
                 :collect
                 `(defun ,scanner-name (,string ,start ,end)
                    ,(format nil "Scanning function for lexer ~S and rule #~D:~2%    ~A"
                             name
                             rule-number
                             (pattern-regex pat)) ; Documentation
                    ,@declarations    ; User declarations
                    (declare (type simple-string ,string)
                             (type non-negative-fixnum ,start ,end))
                    ;; Try the user's supplied compiler, and if it
                    ;; fails, fall back to the PPCRE one.
                    ,(handler-case (funcall (pattern-compiler pat) pat string start end)
                       (scanner-compiler-not-applicable (c)
                         (warn "Attempted to compile a scanner with ~S on the ~
                              regex ~S, but it failed. Falling back to CL-PPCRE."
                               (scanner-compiler-not-applicable-compiler c)
                               (pattern-regex (scanner-compiler-not-applicable-pattern c)))
                         (generate-ppcre-scanner pat string start end))))
                 :unless inline-match-scanners
                   :collect `(declaim (notinline ,scanner-name))

                 ;; Generate the FIRE function.
                 :collect `(declaim (inline ,fire-name))
                 :collect
                 `(defun ,fire-name (,string ,match-start ,match-end ,reg-starts ,reg-ends)
                    ,(format nil "Firing function for lexer ~S and rule #~D:~2%    ~A"
                             name
                             rule-number
                             (pattern-regex pat)) ; Documentation
                    ,@declarations    ; User declarations
                    (declare (type non-negative-fixnum ,match-start ,match-end)
                             (type simple-string ,string)
                             (type vector ,reg-starts ,reg-ends)
                             (ignorable ,string ,reg-starts ,reg-ends))
                    ;; Generate the code to execute for this rule.
                    (block nil
                      ,(generate-pattern-execution-code
                        pat
                        string
                        match-start match-end
                        reg-starts reg-ends)
                      (throw ',sentinel nil)))
                 :unless inline-match-fires
                   :collect `(declaim (notinline ,fire-name)))

             ;; Generate the actual lexer generator.
             (defun ,name (,string &key ((:start ,start) 0) ((:end ,end) (length ,string)))
               ,@(alexandria:ensure-list doc-string)
               ,@declarations
               (check-type ,string string)
               (check-type ,start non-negative-fixnum ":START must be a non-negative fixnum.")
               (check-type ,end non-negative-fixnum ":END must be a non-negative fixnum.")
               (assert (<= ,start ,end) (,start ,end) ":END must be not be less than :START.")
               (let ((,string (if (simple-string-p ,string)
                                  ,string
                                  (coerce ,string 'simple-string))))
                 (declare (type (or null simple-string) ,string))
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
                                            name
                                            pat EXECUTE-TAG
                                            string start end
                                            match-start match-end
                                            reg-starts reg-ends
                                            max-match-length match-rule-index i))
                          ;; If we reached here and didn't match, then
                          ;; the MATCH-RULE-INDEX will still be -1.
                          (when (= -1 ,match-rule-index)
                            (cerror "Continue, returning NIL."
                                    'no-match-error
                                    :lexer-name ',name
                                    :position ,start)
                            (return nil))

                          ,EXECUTE-TAG
                          ;; Update our new start for the next round of
                          ;; matching.
                          (setq ,start ,match-end)

                          ;; Expand into the preferred dispatch
                          ;; strategy. The firing of a rule has a
                          ;; possibility of not returning by way of a
                          ;; THROW.
                          (catch ',sentinel
                            ,(ecase dispatch-mode
                               ((:function-vector)
                                (generate-function-vector-dispatch-code
                                 match-rule-index
                                 patterns
                                 string
                                 match-start
                                 match-end
                                 reg-starts
                                 reg-ends))
                               ((:case)
                                (generate-case-dispatch-code
                                 match-rule-index
                                 patterns
                                 string
                                 match-start
                                 match-end
                                 reg-starts
                                 reg-ends))))
                          ;; If we caught the sentinel, then we
                          ;; continue with the lex loop.
                          (go ,CONTINUE-TAG)))))))))))))
