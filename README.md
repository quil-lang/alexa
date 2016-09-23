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

* Bound variables `$<` and `$>` to determine the start and end of your matches.
* Bound variables to matches on named registers. For example, `(?<DIGIT>\d)` will match a digit, and bind it in your Lisp code to the lexical variable `$DIGIT`.
* Regex aliases. You don't need to re-type `[A-Za-z][A-Za-z0-9_]` for each identifier. You can instead define the alias `(:ident "[A-Za-z][A-Za-z0-9_]")` and use `{{IDENT}}` in your lexical rules.

## Example

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

## Contributing

If you have suggestions or questions, please file an issue in GitHub. If you have any bug fixes or improvements, please make a pull request.

## License and Copyright

This software is released under the BSD 3-clause license. See `LICENSE.txt` for details.

Copyright © 2016 Rigetti Computing