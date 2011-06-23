#!r6rs

(import (rnrs)
        (imi parser lexer-syntax)
        (imi io string)
        (srfi :64))

(define (rule-tester rule)
  (elim-newline)
  (let ([input (get-line (current-input-port))])
    (with-input-from-string
      input
      (lambda ()
        (parse rule)))))


(define (elim-newline)
  (when (char=? #\newline (peek-char))
    (read-char)
    (elim-newline)))


(define lex-sexpr
  (lexer parse-main

    (whitespace
      #\space #\newline #\tab)

    (comment-begin . #\;)
    (special-begin . #\#)
    (char-begin    . #\\)
    (symbol-delim  . #\|)
    (string-delim  . #\")
    (string-escape . #\\)
    (list-begin    . #\()
    (list-end      . #\))

    (delimiter
      list-begin
      list-end
      string-delim
      special-begin
      comment-begin
      whitespace
      %eof)


    (parse-main
      #('whitespace parse-main)
      ,@(comment elim-comment)
      ,@(special special)
      ,@(list list)
      ,@(string string)
      ,@(symbol symbol)
      %eof
      )

    (elim-whitespace
      #(whitespace elim-whitespace)
      #())

    (elim-comment
      #(comment-begin elim-comment-main))
    (elim-comment-main
      '(#\newline %eof)
      #(%any elim-comment-main))

    (special
      #('special-begin special-main))
    (special-main
      #('comment-begin ,@(comment parse-main))
      #('char-begin ,@(char char))
      )

    (char
      delimiter
      symbol)

    (list
      #('list-begin 'elim-whitespace list-main))
    (list-main
      #('list-end)
      #(%eof (%error parse-list "eof while reading list"))
      #(dot 'elim-whitespace list-main)
      #(parse-main 'elim-whitespace list-main))

    (dot
      ,@(dot dot-main0))
    (dot-main0
      #(#\. (`delimiter dot-elipsis)))
    (dot-elipsis
      #(#\. dot-elipsis-end)
      (%error 'elipsis "invalid character in elipsis" %any))
    (dot-elipsis-end
      #(#\. (`delimiter
             (%error 'elipsis "invalid character after elipsis" %any)))
      #(`delimiter (%error 'elipsis "unexpected end of elipsis" %any))
      (%error 'elipsis "invalid character in elipsis" %any))

    (string
      #('string-delim string-main))
    (string-main
      #(string-escape string-main-escape)
      #('string-delim)
      #(%eof (%error parse-string "eof while reading string"))
      #(%any string-main))
    (string-main-escape
      #(%any string-main))

    (symbol
      #('symbol-delim symbol-special)
      #((%not delimiter) symbol-main))
    (symbol-main
      `delimiter
      #(%any symbol-main))
    (symbol-special
      'symbol-delim
      #(%eof (%error parse-symbol "eof while reading escaped symbol"))
      #(%any symbol-special))
      ))






(define lex-comment
  '(#('elim-whitespace ,@simple-comment)

    (whitespace
      #\space #\newline #\tab)
    (simple-comment-begin . #\;)
    (simple-comment-end #\newline %eof)

    (simple-comment
      #(simple-comment-begin simple-comment-main))
    (simple-comment-main
      'simple-comment-end
      #(%any simple-comment-main))

    (elim-whitespace
      #(whitespace elim-whitespace)
      #())
    ))



(define-syntax val
  (syntax-rules ()
    [(_ expr)
     expr
     #;
     (call-with-values
       (lambda () expr)
       list)]))


(define (test-rule rule str defs)
  (with-input-from-string
    str
    (lambda ()
      (parse-rule rule 0 defs))))

(define (test-rule* rule str)
  (with-input-from-string
    str
    (lambda ()
      (parse rule))))

(define (test-lex lex str)
  (with-input-from-string str lex))


#|
(test-begin "parser-tests")
(test-equal 'char '(1 #\a) (val (test-rule '#\a "acbkvaje" '())))
(test-equal 'bare-char '(1 #\d) (val (test-rule '#\d "d" '())))
(test-equal 'any '(1 #\l) (val (test-rule '%any "lasd" '())))
(test-equal 'list0 '(1 #\r) (val (test-rule '(#\r #\x #\y) "r" '())))
(test-equal 'list1 '(1 #\x) (val (test-rule '(#\r #\x #\y) "x" '())))
(test-equal 'list2 '(1 #\y) (val (test-rule '(#\r #\x #\y) "y" '())))
(test-equal 'list-in-list '(1 #\a) (val (test-rule '((#\r #\a) #\x #\y) "a" '())))
(test-equal 'eof `(0 ,(eof-object)) (val (test-rule '%eof "" '())))
(test-equal 'empty-vector '(0 ()) (val (test-rule '#() "asd" '())))
(test-equal 'bare-empty-vector '(0 ()) (val (test-rule '#() "" '())))
(test-equal 'vector '(3 (#\a #\b #\c)) (val (test-rule '#(#\a #\b #\c) "abcd" '())))
(test-equal 'symbol-elim-space '(3 (#\space #\space #\space)) (val (test-rule 'space "   abc" '((space #(#\space space) #())))))
(test-end "parser-tests")
|#



(test-begin "parser-tests")
(test-equal 'char          '#\a (test-lex (lexer #\a)  "acbkvaje"))
(test-equal 'bare-char     '#\d (test-lex (lexer #\d)  "d"))
(test-equal 'any           '#\l (test-lex (lexer %any) "lasd"))
(test-equal 'list0         '#\r (test-lex (lexer (#\r #\x #\y)) "r"))
(test-equal 'list1         '#\x (test-lex (lexer (#\r #\x #\y)) "x"))
(test-equal 'list2         '#\y (test-lex (lexer (#\r #\x #\y)) "y"))
(test-equal 'list-in-list  '#\a (test-lex (lexer ((#\r #\a) #\x #\y)) "a"))
(test-equal 'eof   (eof-object) (test-lex (lexer %eof) ""))
(test-equal 'empty-vector  '()  (test-lex (lexer #()) "asd"))
(test-equal 'bare-empty-vector '() (test-lex (lexer #()) ""))
(test-equal 'vector '(#\a #\b #\c) (test-lex (lexer #(#\a #\b #\c)) "abcd"))
(test-equal 'symbol-elim-space '(#\space #\space #\space) (test-lex (lexer space (space #(#\space space) #())) "   abc"))
(test-end "parser-tests")
