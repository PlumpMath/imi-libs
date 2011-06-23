#!r6rs

(import (rnrs)
        (match)
        (srfi :64)
        (imi io string)
        (imi parser lexer)
        (only (imi parser lexer-syntax) lexer))

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


(define (readfile file)
  (with-input-from-file
    file
    (lambda ()
      (let loop ()
        (if (eof-object? (peek-char))
            (list (lex lex-sexpr))
            (cons (lex lex-sexpr)
                  (loop)))))))

(define (stdreadfile file)
  (with-input-from-file file read))

(define (readfile* file)
  (with-input-from-file
    file
    read-sexpr
    #;
    (lambda ()
      (let loop ()
        (if (eof-object? (peek-char))
          (list (read-sexpr))
          (cons (read-sexpr)
                (loop)))))))

(define (mute . sth) '())

(define (test* . files)
  (time (map readfile* files)))

(define (stdtest . files)
  (time (map stdreadfile files)))



(define read-sexpr
  (lexer parse-all

         (delimiter
           list-begin
           list-end
           string-delim
           special-begin
           comment-begin
           whitespace
           %eof)


         (parse-all
           %eof
           #(parse-main parse-all))

         (parse-main
           #(`whitespace 'elim-whitespace parse-main)
           ',@(comment 'simple-comment)
           special
           quotation-form
           ,@(list list)
           ,@(string string)
           ,@(number number)
           ,@(symbol symbol)

           %eof
           )


         (whitespace
           #\space #\newline #\tab)
         (elim-whitespace
           #(whitespace elim-whitespace)
           #())


         (comment-begin . #\;)
         (comment-end #\newline %eof)
         (simple-comment
           #(comment-begin 'simple-comment-main))
         (simple-comment-main
           'comment-end
           #(%any 'simple-comment-main))



         (block-comment
           #(#\# block-comment-inner block-comment)
           #(#\| block-comment-end)
           #(%any block-comment))
         (block-comment-inner
           #(#\| block-comment)
           #(%any))
         (block-comment-end
           #(#\#)
           #(%any block-comment))


         (special-begin . #\#)
         (special
           #('special-begin special-main))
         (special-main
           ;#('comment-begin ,@(comment parse-main))
           '#('comment-begin ,@(comment 'parse-main))
           #('char-begin ,@(char char))
           ,@(vector list)
           '#(#\! ,@(reader-special symbol))
           ;#(#\| ,@(block-comment block-comment))
           '#('#\| ,@(comment 'block-comment))
           ,@(boolean boolean)
           ,@(syntax quotation-form)
           (%error special "invalid special" %any))

         (boolean
           (#\t `delimiter)
           (#\f `delimiter))

         (char-begin . #\\)
         (char delimiter symbol)



         (digit #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         (number-special #\. #\+ #\-)
         (number
           number-sign
           number-main)
         (number-main
           #(#\. number-comma)
           #(digit number-main)
           `delimiter
           (%error 'read-number "invalid character in number" %any))
         (number-comma
           #(digit number-comma)
           `delimiter
           (%error 'read-number "invalid character in number" %any))
         (symbol-plus ,@(plus `delimiter))
         (symbol-minus ,@(minus `delimiter))
         (number-sign
           #(#\+ (symbol-plus number-main))
           #(#\- (symbol-minus number-main)))



         (quotation-form
           #('#\' ,@(quote parse-main))
           #('#\` ,@(quasiquote parse-main))
           #('#\, unquote-form))
         (unquote-form
           #('#\@ ,@(unquote-splicing parse-main))
           ,@(unquote parse-main))



         (list-begin #\( #\[)
         (list-end   #\) #\])
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



         (string-delim  . #\")
         (string-escape . #\\)
         (string
           #('string-delim string-main))
         (string-main
           #(string-escape string-main-escape)
           #('string-delim)
           #(%eof (%error parse-string "eof while reading string"))
           #(%any string-main))
         (string-main-escape
           #(%any string-main))



         (symbol-delim  . #\|)
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
           )
  )


(define lex-sexpr
  '(parse-main

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
      quotation-form
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
      ,@(vector list)
      #(#\! ,@(reader-special symbol))
      ,@(boolean boolean)
      )
    (boolean
      (#\t `delimiter)
      (#\f `delimiter))

    (char
      delimiter
      symbol)

    (quotation-form
      #('#\' ,@(quote parse-main))
      #('#\` ,@(quasiquote parse-main))
      #('#\, unquote-form))
    (unquote-form
      #('#\@ ,@(unquote-splicing parse-main))
      ,@(unquote parse-main))

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



(define (test-rule rule str)
  (with-input-from-string
    str
    (lambda ()
      (lex rule))))

(define (res-stringify res)
  (cond
    [(position? res)
     (match res
       [(,name ,start ,end ,cont)
        (list name start end (res-stringify cont))]
       [(,start ,end ,cont)
        (list start end (res-stringify cont))]
       [,invalid
        (error 'res-stringify
               "unknown position form"
               invalid)])]
    [(list? res)
     (if (for-all char? res)
         (list->string res)
         (map res-stringify res))]
    [(char? res)
     (string res)]
    [(eof-object? res) res]
    [else (error 'res-stringify
                 "invalid result"
                 res)]))

(define (test-rule* rule str)
  (res-stringify (test-rule rule str)))


(test-begin "lexer")
  (test-equal 'char      "a" (test-rule* '(#\a) "acbkvaje"))
  (test-equal 'bare-char "d" (test-rule* '(#\d) "d"))

  (test-equal 'any "l" (test-rule* '(%any) "lasd"))

  (test-equal 'list0 "r" (test-rule* '((#\r #\x #\y)) "r"))
  (test-equal 'list1 "x" (test-rule* '((#\r #\x #\y)) "x"))
  (test-equal 'list2 "y" (test-rule* '((#\r #\x #\y)) "y"))

  (test-equal 'list-in-list "a" (test-rule* '(((#\r #\a) #\x #\y)) "a"))

  (test-equal 'eof (eof-object) (test-rule* '(%eof) ""))

  (test-equal 'empty-vector      ""    (test-rule* '(#()) "asd"))
  (test-equal 'bare-empty-vector ""    (test-rule* '(#()) ""))
  (test-equal 'vector            "abc" (test-rule* '(#(#\a #\b #\c)) "abcd"))

  (let ([lex-elim-space '(elim-space
                          (elim-space
                            #(#\space elim-space)
                            #()))])

    (test-equal 'symbol-elim-space      "   " (test-rule* lex-elim-space "   abc"))
    (test-equal 'symbol-elim-space-bare "  "  (test-rule* lex-elim-space "  "))
    (test-equal 'symbol-elim-space-none ""    (test-rule* lex-elim-space "a"))
    )
(test-end "lexer")
