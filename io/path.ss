(library (imi io path)
  (export relative-path
          absolute-path
          path->string
          path-append)
  (import (rnrs)
          (imi list)
          (imi list processing)
          (imi string processing)
          (imi sugar case-pred)
          (imi proc compose)
          (imi proc predicate logic)
          (imi utils tester))

  ;;; FIXME: path: differ between directory and file

  (define-record-type path (fields absolute? path))

  (define (relative-path ls)
    (make-path
      #f
      (internal-path ls)))

  (define (absolute-path ls)
    (make-path
      #t
      (internal-path ls)))

  (define (internal-path obj)
    (if (path? obj)
      (path-path obj)
      (reverse 
        (case-pred obj
          [(string?)
            (string-split (char-in "/") obj)]
          [(symbol?)
            (list (symbol->string obj))]
          [(list?)
            (map path-elem->string obj)]
          [else
            (error 'path "expecting string, symbol or list as path" obj)]))))

  (define (path-elem->string e)
    (case-pred e
      [(string?) e]
      [(symbol?) (symbol->string e)]
      [(list?)
        (apply string-append
               (list-intersperse
                 "."
                 (map path-elem->string e)))]
      [else
        (error 'path-elem->string "don't know how to convert" e)]))


  (define (path->string p)
    (apply string-append
           (if (path-absolute? p) "/" "./")
           (list-intersperse
             "/"
             (reverse (path-path p)))))

  (define (collapse-dir-up ls)
    (cond
      [(null? ls) ls]
      [(string=? (car ls) "..")
       (if (length< ls 2)
         '()
         (collapse-dir-up (cddr ls)))]
      [else
        (cons (car ls)
              (collapse-dir-up (cdr ls)))]))

  (define (path-append first . rest)
    (when (exists (and/p path? path-absolute?) rest)
      (error 'path-append "paths appended have to be relative" rest))

    (let ([base (internal-path first)]
          [subpaths (map internal-path rest)])
      (make-path
        (case-pred first
          [(path?) (path-absolute? first)]
          [(string?) (char=? #\/ (string-ref first 0))]
          [else #f])
        (collapse-dir-up
          (apply append
                 (reverse (cons base subpaths)))))))

  )
