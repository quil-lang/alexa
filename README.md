# ALEXA: A Lexical Analyzer Generator

## Introduction

ALEXA is a tool similar to `lex` or `flex` for generating lexical analyzers. Unlike tools like `lex`, however, ALEXA defines a domain-specific language within your Lisp program, so you don't need to invoke a separate tool.

The `alexa` package exports a single macro: `define-string-lexer`. You can view the documentation for this macro using the standard Common Lisp facilities, e.g., `describe` or

```
(format t "~A"
        (documentation 'alexa:define-string-lexer
                       'function))
```

ALEXA may be used with parsing libraries such as [`cl-yacc`](https://www.irif.univ-paris-diderot.fr/~jch/software/cl-yacc/).

## Special Features

ALEXA has a few unique features to make lexical analysis easier. These include:

* Bound variables `$<` and `$>` to determine the start and end of your matches. The value of `(- $> $<)` is equal to the length of the match.
* Bound variables to matches on named registers. For example, `(?<DIGIT>\d)` will match a digit, and bind it in your Lisp code to the lexical variable `$DIGIT`.
* Regex aliases. You don't need to re-type `[A-Za-z][A-Za-z0-9_]` for each identifier. You can instead define the alias `(:ident "[A-Za-z][A-Za-z0-9_]")` and use `{{IDENT}}` in your lexical rules.
* Eager matching. Normally ALEXA looks for the longest match. For optimization, you may decide to execute an action if a match succeeds. This can be done by declaring a regular expression as `EAGER`.

ALEXA has been written with efficiency in mind. Generated lexers will avoid consing in most cases, unless you use the `$@` variable which expands into a `SUBSEQ` call on the string.

## Example

### Arithmetic Expression Tokenization

The following simple example shows how to tokenize simple arithmetic expressions. First, we define what a token is and how to make one.

```
(deftype token ()
  `(cons keyword t))

(defun tok (type &optional val)
  (cons type val))
```

Advanced applications may opt to store other information in their token data structure, such as token position in the string (which can be extracted with `$<` and `$>`).

Next, we define the lexer. We create two aliases `:num` and `:name` to make our lexical rules a little bit easier to read.


```
(alexa:define-string-lexer arith-lexer
  "Make a lexical analyzer for arithmetic expressions."
  ((:num "\\d+")
   (:name "[A-Za-z][A-Za-z0-9_]*"))
  ("pi"       (return (tok :number pi)))
  ("{{NAME}}" (return (tok :variable (intern $@))))
  ("{{NUM}}"  (return (tok :number (parse-integer $@))))
  ("[+*/-]"   (return (tok :operator (intern $@ 'keyword))))
  ("\\("      (return (tok :left-paren)))
  ("\\)"      (return (tok :right-paren)))
  ("\\s+"     nil))
```

To use the lexer, we make one by calling `arith-lexer` on the string being lexically analyzed. To get the next token, we just `funcall` our lexer. We can make a small helper function to lex an entire string until the lexer has been exhausted.

```
(defun lex-line (string)
  (loop :with lexer := (arith-lexer string)
        :for tok := (funcall lexer)
        :while tok
          :collect tok))
```

Calling `lex-line` on arithmetic expressions now gives us our expected results.

```
> (lex-line "2*(x+1)/z")
((:NUMBER . 2)
 (:OPERATOR . :*)
 (:LEFT-PAREN)
 (:VARIABLE . |x|)
 (:OPERATOR . :+)
 (:NUMBER . 1)
 (:RIGHT-PAREN)
 (:OPERATOR . :/)
 (:VARIABLE . |z|))

> (lex-line "1/(1/R_1 + 1/R_2)")
((:NUMBER . 1)
 (:OPERATOR . :/)
 (:LEFT-PAREN)
 (:NUMBER . 1)
 (:OPERATOR . :/)
 (:VARIABLE . R_1)
 (:OPERATOR . :+)
 (:NUMBER . 1)
 (:OPERATOR . :/)
 (:VARIABLE . R_2)
 (:RIGHT-PAREN))
```

In our lexer, we have `pi` as the rule for one of the lexical actions. ALEXA will generate code to correctly identify it. ALEXA follows the rule of matching the longest string possible, breaking ties depending on the order of the rules stated in the lexer's definition. In the following example, we have both `pi` and `pip`. Even though two rules match `pi`, the first one breaks the tie and we correctly resolve it to `3.141...`. Similarly, the lexer matches `pip` against the correct rule because it was the longest match.

```
> (lex-line "pi+2*pip")

((:NUMBER . 3.141592653589793d0)
 (:OPERATOR . :+)
 (:NUMBER . 2)
 (:OPERATOR . :*)
 (:VARIABLE . |pip|))
```

Note that ALEXA has no notion of "specificity". If the rules for `pi` and `{{NAME}}` were flipped, then `{{NAME}}` would always supersede `pi`.

### Eager Matching

In our arithmetic expression lexer, we can optimize the lexing process. If we match against any of the latter six rules, then we know the match is unambiguous and we can fire the rule. ALEXA is very conservative, and has no idea about this. You can tell ALEXA that this is the case by declaring said rules as `EAGER`. In addition, we can re-order the rules favorably. We get the equivalent lexer:

```
(alexa:define-string-lexer arith-lexer-opt
  "Make a lexical analyzer for arithmetic expressions."
  ((:num "\\d+")
   (:name "[A-Za-z][A-Za-z0-9_]*"))
  ((eager "{{NUM}}")  (return (tok :number (parse-integer $@))))
  ((eager "[+*/-]")   (return (tok :operator (intern $@ 'keyword))))
  ((eager "\\(")      (return (tok :left-paren)))
  ((eager "\\)")      (return (tok :right-paren)))
  ((eager "\\s+")     nil)
  ("pi"               (return (tok :number pi)))
  ((eager "{{NAME}}") (return (tok :variable (intern $@)))))
```

As a microbenchmark, we generate random strings of tokenizable content.

```
(defun lex (lexer)
  (loop :for tok := (funcall lexer)
        :while tok
          :collect tok))

(defun random-word ()
  (substitute #\_ #\- (format nil "~R" (random 1000))))

(defun random-expr (n)
  (with-output-to-string (s)
    (loop :repeat n :do
      (alexandria:whichever
       (format s "~D" (random most-positive-fixnum))
       (format s "~C" (alexandria:random-elt "+-*/"))
       (write-string "pi " s)
       (write-string (random-word) s)
       (format s "(")
       (format s ")")
       (loop :repeat (random 8) :do (write-char #\Space s))))))

(defun test (&optional (string (random-expr 1000000)))
  (format t "Length of string: ~D~%" (length string))
  (let ((l (arith-lexer string))
        (lo (arith-lexer-opt string)))
    (sb-ext:gc :full t)
    (time (lex l))
    (sb-ext:gc :full t)
    (time (lex lo))
    nil))
```

Calling `test` on `SBCL 1.4.7.192-8b8c9bcc0` gives results like

```
Length of string: 7037267
Evaluation took:
  2.276 seconds of real time
  2.276657 seconds of total run time (2.274266 user, 0.002391 system)
  100.04% CPU
  6,357,950,986 processor cycles
  0 bytes consed

Evaluation took:
  1.571 seconds of real time
  1.572094 seconds of total run time (1.570309 user, 0.001785 system)
  100.06% CPU
  4,389,463,410 processor cycles
  0 bytes consed
```

In this example, we reduce the runtime by about 31%.

## Contributing

If you have suggestions or questions, please file an issue in GitHub. If you have any bug fixes or improvements, please make a pull request.

## License and Copyright

This software is released under the BSD 3-clause license. See `LICENSE.txt` for details.

Copyright © 2016–2018 Rigetti Computing