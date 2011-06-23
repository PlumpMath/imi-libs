#!r6rs

(library (imi string regex)
  (export irregex
          match
          compile
          irregex-simple-string?
          irregex-simple-string)
  (import (rnrs)
          (rnrs eval))

  ;;; environment for matcher funcs dynamically created by eval
  (define regex-env (environment '(rnrs) '(srfi :14) 
                                 '(only (srfi :13) reverse-list->string)))

  ;;; compile matcher func with helper code
  ;;;   done by eval and regex-env
  (define (compile expr)
    (eval `(lambda (str)
             ;;; helper procedure
             (define (add1 x)
               (+ x 1))

             ;;; create match structure with information about content and position
             (define (make-match content start end)
               (cons content
                     (cons start end)))

             (let ([input-length (string-length str)])  ;;; input length for readability
                                                        ;;;  and efficiency calculated here

               (let loop ([ch (string->list str)]       ;;; input character "stream"
                          [pos 0]                       ;;; position in "stream"
                          [matches '()])                ;;; found matches

                 ;;; add new match and search on
                 (define (found-match content end)
                   (loop (cdr ch) (add1 pos)
                         (cons (make-match content pos end) matches)))
                 
                 ;;; add new matches and search on
                 (define (found-matches m)
                   (loop (cdr ch) (add1 pos)
                         (append m matches)))
 
                 ;;; search on without new match
                 (define (search-on)
                   (loop (cdr ch) (add1 pos) matches))
                
                 ;;; return matches, if there are no matches
                 ;;;   return #f for easier use with if
                 (define (return-matches)
                   (and (not (null? matches))
                        matches))

                 (cond
                   ;;; at the end of "stream"
                   [(null? ch)
                     (return-matches)]
                   ;;; search for matches as described
                   [else
                     ,expr]))))
          regex-env))

  ;;; match compiled irregex in str
  (define (match irregex str)
    (irregex str))

  ;;; compile any irregex expression
  (define (irregex rexpr)
    (compile (irregex-code rexpr)))

  ;;; crete code to any irregex expression
  (define (irregex-code rexpr)
    (cond
      [(irregex-simple-string? rexpr)
        (irregex-simple-string rexpr)]
      [(irregex-repeat-range? rexpr)
        (irregex-repeat-range rexpr)]
      [else
        (error 'irregex-code "not a valid irregex expression" rexpr)]))


  ;;; irregex expression for simple whole matching strings
  (define (irregex-simple-string? rexpr)
    (string? rexpr))

  ;;; create code for searching for a simple string
  (define (irregex-simple-string rexpr)
    `(if (<= pos (- input-length (string-length ,rexpr)))  ;;; last position to be searched at
       ;;; search for match
       (let loop ([ch ch]                      ;;; string searched in
                  [pos pos]                    ;;; position in string searched in
                  [str (quote ,(string->list rexpr))]  ;;; string searched for
                  [match-content '()])         ;;; content of match

         ;;; update match content
         (define (trace-match)
           (cons (car ch) match-content))

         (cond
           ;;; string searched for at the end -> found match
           [(null? str)
             (found-match match-content pos)]
           ;;; string searched in at the end -> no match found
           [(null? ch)
             (search-on)]
           ;;; search for match here or, if not already comparing, at next character
           [else
             (if (char=? (car ch) (car str))
               (loop (cdr ch) (add1 pos)
                     (cdr str)
                     (trace-match))
               (search-on))]))

       ;;; position too near at the end, string searched for can't be there, doesn't fit there
       (search-on)))

  ;;; irregex expression for repeating
  (define (irregex-repeat-range? rexpr)
    (and (list rexpr) (= (length rexpr) 4)
         (eq? (car rexpr) '**)
         (irregex-repeat-range-spec? (cadr rexpr))
         (irregex-repeat-range-spec? (caddr rexpr))))

  ;;; irregex specification of range
  (define (irregex-repeat-range-spec? x)
    (or (and (symbol? x) (eq? x '*))
        (and (integer? x) (>= x 0))))

  ;;; create code for repeating regex
  ;;; FIXME: something doesn't work :-D
  (define (irregex-repeat-range rexpr)
    (let ([from (cadr rexpr)] [to (caddr rexpr)]
          [sub-rexpr (irregex-code (cadddr rexpr))])
      (define (specified? x)
        (and (integer? x) (> x 0)))
      (define (unspecified? x)
        (and (symbol? x) (eq? x '*)))

      `(let ([top-found-match   found-match]
             [top-found-matches found-matches]
             [top-search-on     search-on]
             [start pos])
         (define (in-range? x)
           ,(cond
              [(and (specified? from) (specified? to))
                `(< ,from x ,to)]
              [(and (unspecified? from) (specified? to))
                `(< x ,to)]
              [(and (specified? from) (unspecified? to))
                `(< ,from x)]
              [(and (unspecified? from) (unspecified? to))
                '#t]))

         (let loop ([ch ch]
                    [pos pos]
                    [repeats 0]
                    [match-content '()]
                    [matches '()])

           (define (found-match content end)
             (cond
               [(in-range? (add1 repeats))
                 (let ([match-content (append content match-content)])
                   (loop (list-tail ch (- end pos 1)) (add1 end) (add1 repeats)
                         match-content
                         (cons (make-match match-content start end) matches)))]
               [(in-range? repeats)
                 (top-found-matches matches)]
               [else
                 (loop (list-tail ch (- end pos 1)) (add1 end) (add1 repeats)
                       (append content match-content)
                       matches)]))

           (define (search-on)
             (cond
               [(in-range? repeats)
                 (top-found-matches matches)]
               [else
                 (top-search-on)]))

           ,sub-rexpr))))

  )
