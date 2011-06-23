
(import (rnrs)
        (imi sugar receive)
        (imi io string)
        (imi match utils)
        (imi parser lexer-syntax)
        (srfi :64))




(define rd
  (lexer #(`whitespace char)

         (whitespace . #\newline)

         (char . %any)

         (symbol
           #\newline
           #(%any symbol))
         ))

(define read-sexpr
  #;
  (lexer #('elim-whitespace symbol)

         (whitespace
           #\space #\tab #\newline)

         (elim-whitespace
           #(whitespace elim-whitespace)
           #())

         (symbol
           `whitespace
           #(%any symbol))
         )

  ;#;
  (lexer parse-main

         (whitespace
           #\space #\tab #\newline)

         (delimiter #\( whitespace #\; %eof)

         (parse-main
           #(`whitespace 'elim-whitespace parse-main)
           ,@(symbol symbol))

         (elim-whitespace
           #(whitespace elim-whitespace)
           #())

         (symbol
           #(%any symbol-main))
         (symbol-main
           `delimiter
           #(%any symbol-main))
         ))

#;
(test-group "lexer matching"
  (test-eq 'char-same #t (matches-rule? '#\a #\a '()))
  (test-eq 'char-diff #f (matches-rule? '#\a #\b '()))
  (test-eq 'char-eof  #f (matches-rule? '#\a (eof-object) '()))

  (test-eq 'any-char #t (matches-rule? '%any #\a '()))
  (test-eq 'any-eof  #f (matches-rule? '%any (eof-object) '()))

  (test-eq 'eof-char #f (matches-rule? '%eof #\x '()))
  (test-eq 'eof-eof  #t (matches-rule? '%eof (eof-object) '()))

  (test-eq 'name-char-same #t (matches-rule? 'name #\a '((name . #\a))))
  (test-eq 'name-char-diff #f (matches-rule? 'name #\b '((name . #\a))))

  (test-eq 'pair-car-char #t (matches-rule? '(#\a . #\b) #\a '()))
  (test-eq 'pair-cdr-char #t (matches-rule? '(#\a . #\b) #\b '()))
  (test-eq 'pair-none     #f (matches-rule? '(#\a . #\b) #\c '()))
  (test-eq 'pair-none-eof #f (matches-rule? '(#\a . #\b) (eof-object) '()))

  (test-eq 'null-char #f (matches-rule? '() #\a '()))
  (test-eq 'null-eof  #f (matches-rule? '() (eof-object) '()))

  (test-eq 'vector-empty-char #t (matches-rule? '#() #\a '()))
  (test-eq 'vector-empty-eof  #t (matches-rule? '#() (eof-object) '()))
  (test-eq 'vector-char-same  #t (matches-rule? '#(#\a) #\a '()))
  (test-eq 'vector-char-diff  #f (matches-rule? '#(#\a) #\b '()))
  )
