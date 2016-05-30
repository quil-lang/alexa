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

(defmacro define-string-lexer (name &body patterns)
  (let ((patterns (loop :for (regex . code) :in patterns
                        :for parse-tree := (let ((cl-ppcre:*allow-named-registers* t))
                                             `(:SEQUENCE :START-ANCHOR ,(cl-ppcre:parse-string regex)))
                        :collect (multiple-value-bind (num-regs names)
                                     (extract-registers parse-tree)
                                   (make-pattern :regex regex
                                                 :parse-tree parse-tree
                                                 :num-registers num-regs
                                                 :register-names names
                                                 :code code)))))
    (alexandria:with-gensyms (continue-tag string start end)
      `(defun ,name (,string &key ((:start ,start) 0) ((:end ,end) (length ,string)))
         (check-type ,start (integer 0) ":START ")
         (check-type ,end (integer 0))
         (assert (<= ,start ,end) (,start ,end) "END must be not be less than START.")
         (let ((cl-ppcre:*allow-named-registers* t))
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
                  (error "Couldn't find match at position ~S" ,start)))))))))
