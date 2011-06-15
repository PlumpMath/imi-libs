(library (imi regex)
  (export string-match
          regex)
  (import (rnrs)
          (imi math)
          (imi regex structs))

  ;;; DRAFT


  ;;;;; REGEX
  ;;; <regex> -> <simple-string>
  ;;;          | <repeat>

  (define (regex rexpr)
    (cond
      [(regex-simple-string? rexpr)
        (regex-simple-string rexpr)]
      [(regex-repeat? rexpr)
        (regex-repeat rexpr)]
      [else (error 'regex "unknown regex" rexpr)]))

  (define (string-match str regexproc)
    (let loop ([str (string->list str)]
               [pos 0]
               [match-list '()])

      (define (return matches)
        (and (not (null? matches)) matches))

      (define (next-or-return matches)
        (if (null? str)
          (return matches)
          (loop (cdr str) (add1 pos)
                matches)))
      
      (define (matches s end content cont)
        (next-or-return
          (cons (make-match content
                            (make-position pos end))
                match-list)))

      (or (regexproc str pos '() matches)
          (next-or-return match-list))))


  ;;;;; SIMPLE STRING
  ;;; <simple-string> -> <string>

  (define (regex-simple-string? rexpr)
    (string? rexpr))

  (define (regex-simple-string rexpr)
    (let ([search (string->list rexpr)])
      (lambda (str pos content matches)
        (let loop ([str str]
                   [pos pos]
                   [search search]
                   [content content])
          (cond
            [(null? search)
              (matches str pos content #f)]
            [(null? str) #f]
            [(char=? (car str) (car search))
              (loop (cdr str) (add1 pos)
                    (cdr search)
                    (cons (car str) content))]
            [else #f])))))


  ;;;;; REPEAT
  ;;; <repeat> -> (** <from> <to> <regex>)
  ;;; <from>   -> <repeat-spec>
  ;;; <to>     -> <repeat-spec>
  ;;; <repeat-spec> -> * | <number>

  (define (regex-repeat? rexpr)
    (and (list? rexpr) (eq? '** (car rexpr))
         (regex-repeat-spec? (cadr rexpr))
         (regex-repeat-spec? (caddr rexpr))))

  (define (regex-repeat-spec? expr)
    (or (and (integer? expr) (> expr 0))
        (eq? expr '*)))

  (define (regex-repeat rexpr)
    (let ([from (cadr rexpr)]
          [to   (caddr rexpr)]
          [subregexproc (regex (cadddr rexpr))])

      (define (unspec? x) (eq? x '*))
      (define (spec? x) (not (unspec? x)))
      (define in-range?
        (cond
          [(and (spec? from) (spec? to))
            (lambda (x) (< from x to))]
          [(and (unspec? from) (spec? to))
            (lambda (x) (< x to))]
          [(and (spec? from) (unspec? to))
            (lambda (x) (< from x))]
          [(and (unspec? from) (unspec? to))
            (lambda (x) #t)]))

      (lambda (str pos content top-matches)
        (let loop ([str str]
                   [pos pos]
                   [content content]
                   [repeats 0])

          (define (matches s end content cont)
            (define (mcont)
              (or (and cont (cont))
                  (loop s end content (add1 repeats))))

            (cond
              [(in-range? (add1 repeats))
                (or (loop s end content (add1 repeats))
                    (top-matches s end content mcont))]
              [(in-range? repeats) #f]
              [else
                (loop s end content (add1 repeats))]))

          (subregexproc str pos content matches)))))

  )
