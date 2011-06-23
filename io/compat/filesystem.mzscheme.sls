#!r6rs

(library (imi io compat filesystem)
  (export directory-list
          file-directory?
          path-exists?)
  (import (rnrs)
          (prefix (racket) rkt:))


  (define (string-as-path str)
    (list->string
      (let loop ([chars (string->list str)])
        (cond
          [(null? chars) '()]
          [(char=? (car chars)
                   #\/)
           (if (and (not (null? (cdr chars)))
                    (char=? #\/ (cadr chars)))
               (loop (cdr chars))
               (cons (car chars)
                     (loop (cdr chars))))]
          [else
           (cons (car chars)
                 (loop (cdr chars)))]))))
          

  (define (rkt-list->scm-list sth)
    (if (rkt:null? sth)
        '()
        (cons (rkt:car sth)
              (rkt-list->scm-list
                (rkt:cdr sth)))))


  (define (directory-list path)
    (map rkt:path->string
         (rkt-list->scm-list
           (rkt:directory-list path))))

  

  (define (file-directory? path)
    (cond
      [(rkt:file-exists? path) #f]
      [(rkt:directory-exists? path) #t]
      [else
       (error 'file-directory?
              "this file doesn't exist"
              path)]))


  (define (path-exists? path)
    (or (rkt:file-exists? path)
        (rkt:directory-exists? path)))



  )

