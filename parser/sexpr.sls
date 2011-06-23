#!r6rs

(library (imi parser sexpr)
  (export read-sexpr)
  (import (rnrs)
          (imi sugar cut)
          (imi sugar receive)
          (imi list processing)
          (imi utils tester)
          (imi parser sexpr-syntax))

  (define read-sexpr
    (case-lambda
      [()    (read-sexpr 0)]
      [(pos) (read-sexpr pos 0)]
      [(pos line)
       (receive (sexpr pos line) (read-sexpr-token '() pos line)
         sexpr)]))

  (define (read-sexpr-token last pos line)
    (receive (c pos line) (read-next-char/pos)
      (read-sexpr-token-from-char c last pos line)))

  (define (read-sexpr-token-from-char c last pos line)
    (cond
      [(reader-special? c)
       (read-sexpr-special c last pos line)]
      [(reader-brace-open? c)
       (read-sexpr-list last pos line)]
      [(reader-string-delimiter? c)
       (read-sexpr-string last pos line)]
      [(reader-dot? c)
       (read-sexpr-dot last pos line)]
      [(reader-symbol-escaper? c)
       (read-sexpr-escaped-symbol last pos line)]
      [(reader-comment? c)
       (read-sexpr-comment last pos line)]
      [(reader-brace-close? c)
       (error 'read-sexpr
              "unexpectet closing paren"
              (read-char-pos pos line))]
      [else
       (read-sexpr-simple c last pos line)]))

  (define whitespace? (char-in *whitespaces*))
  (define reader-special? (char-in *reader-specials*))
  (define reader-brace-open? (char-in "("))
  (define reader-brace-close? (char-in ")"))
  (define reader-string-delimiter? (cut char=? <> #\"))
  (define reader-dot? (char-in "."))
  (define reader-symbol-escaper? (char-in "|"))
  (define reader-comment? (char-in ";"))

  (define *whitespaces* " \t\r\n\p")
  (define *reader-specials* "#`',")


  (define (read-sexpr-special c last pos line)
    (cond
      [(reader-quote? c)
       (read-sexpr-quote last pos line)]
      [(reader-unquote? c)
       (read-sexpr-unquote last pos line)]
      [(reader-quasiquote? c)
       (read-sexpr-quasiquote last pos line)]
      [(reader-special-hash? c)
       (read-sexpr-hash last pos line)]
      [else
       (error 'read-sexpr-special
              "not a special"
              (connect-pos c pos line))]))

  (define (read-sexpr-hash last pos line)
    (let ([relpos (make-relative-position last pos line)])
      (receive (c pos line) (read-char/pos pos line)
        (cond
          [(reader-brace-open? c)
           (read-sexpr-vector relpos pos line)]
          [(reader-number-system-indicator? c)
           (read-special-number relpos pos line)]
          [(reader-bang? c)
           (eat-up-reader-special last pos line)]
          [(reader-syntax-special? c)
           (read-syntax-special relpos pos line)]
          [(reader-comment? c)
           (read-datum-comment pos line)]
          [else
           (error 'read-sexpr
                  "unknown reader special"
                  (connect-pos c pos line))]))))

  (define reader-quote? (char-in "'"))
  (define reader-unquote? (char-in ","))
  (define reader-quasiquote? (char-in "`"))
  (define reader-special-hash? (char-in "#"))
  (define reader-number-system-indicator? (char-in "xob"))
  (define reader-bang? (char-in "!"))
  (define reader-syntax-special? (char-in "',`"))


  (define (read-sexpr-list last pos line)
    (let ([relpos (make-relative-position last pos line)])
      (receive (first pos line) (read-sexpr-token relpos pos line)
        (receive (rest pos line) (read-sexpr-list-rest first pos line)
          (values (cons/pos relpos first rest)
                  pos
                  line)))))

  (define (read-sexpr-list-rest last pos line)
    (receive (c pos line) (read-next-char/pos pos line)
      (let ([relpos (make-relative-position last pos line)])
        (cond
          [(reader-brace-close? c)
           (values (null/pos relpos)
                   pos
                   line)]
          [(reader-dot? c)
           (if (end-of-token?)
               (receive (pos line) (eat-up whitespace? pos line)
                 (let ([rest (read-sexpr-token last pos line)])
                   (receive (c pos line) (read-next-char/pos pos line)
                     (if (reader-close-brace? c)
                         rest
                         (error 'read-sexpr
                                "after a '.' only one element is allowed"
                                (connect-pos c pos line))))))
               (receive (second pos line) (read-sexpr-dot last pos line)
                 (receive (rest pos line)
                          (read-sexpr-list-rest relpos pos line)
                   (values (cons/pos relpos second rest)
                           pos
                           line))))]
          [else
           (receive (second pos line)
                    (read-sexpr-token-from-char c relpos pos line)
             (receive (rest pos line)
                      (read-sexpr-list-rest (position second)
                                            pos
                                            line)
               (values (cons/pos relpos second rest)
                       pos
                       line)))]))))

  
  (define (read-sexpr-string last pos line)
    (let ([relpos (make-relative-position last pos line)])
      (let loop ([chars '()]
                 [pos pos]
                 [line line])
        (receive (c pos line) (read-char/pos pos line)
          (cond
            [(reader-escape? c)
             (receive (c pos line) (read-char/pos pos line)
               (loop (cons (or (string-escape c)
                               (error 'string-escape
                                      "invalid string escape"
                                      (connect-pos c pos line)))
                           chars)
                     pos
                     line))]
            [(reader-string-delimiter? c)
             (values (list->string (reverse chars))
                     pos
                     line)]
            [else
             (loop (cons c chars)
                   pos
                   line)])))))

  (define (string-escape char)
    (let ([x (assq char *escape-table*)])
      (and x (cdr x))))

  (define reader-escape? (cut char=? <> #\\))

  (define *escape-table*
    (map cons 
         (string->list "abnprtv")
         (string->list "\a\b\n\p\r\t\v")))


  (define (read-sexpr-dot last pos line)
    (cond
      [(end-of-token?)
       (error 'read-sexpr
              "invalid '.', can only be before the last element of a list"
              (make-position pos line))]
      [else
       (read-sexpr-simple #\. last pos line)]))


  (define (read-sexpr-escaped-symbol last pos line)
    (let ([relpos (make-relative-position last pos line)])
      (let loop ([chars '()]
                 [pos pos]
                 [line line])
        (receive (c pos line) (read-char/pos pos line)
          (cond
            [(reader-escape? c)
             (receive (c pos line) (read-char/pos pos line)
               (loop (cons c chars)
                     pos
                     line))]
            [(reader-symbol-escaper? c)
             (values (string->symbol
                       (list->string
                         (reverse chars)))
                     pos
                     line)]
            [else
             (loop (cons c chars)
                   pos
                   line)])))))

  
  (define (read-sexpr-comment last pos line)
    (let loop ([pos pos]
               [line line])
      (receive (c pos line) (read-char/pos pos line)
        (cond
          [(reader-newline? c)
           (read-sexpr-token last pos line)]
          [else
           (loop pos line)]))))

  (define (read-sexpr-simple c last pos line)
    (let ([relpos (make-relative-position last pos line)])
      (let loop ([chars (list c)]
                 [pos pos]
                 [line line])
        (cond
          [(end-of-token?)
           (let ([str (list->string chars)])
             (or (string->number str)
                 (string->symbol str)))]
          [else
           (receive (c pos line) (read-char/pos pos line)
             (loop (cons c chars)
                   pos
                   line))]))))


  )

