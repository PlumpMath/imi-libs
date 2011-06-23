#!r6rs

(library (imi list set)
  (export list-union
          list-intersection
          list-difference)
  (import (rnrs)
          (imi sugar cut))


  (define (list-union eq? ls0 ls1)
    (cond
      [(null? ls0) ls1]
      [(null? ls1) ls0]
      [(memp (cut eq? <> (car ls0))
             ls1)
       (list-union eq? (cdr ls0) ls1)]
      [else
       (list-union eq?
                   (cdr ls0)
                   (cons (car ls0)
                         ls1))]))


  (define (list-intersection eq? ls0 ls1)
    (filter (lambda (elem)
              (memp (cut eq? elem <>) ls1))
            ls0))



  (define (list-difference eq? ls0 ls1)
    (cond
      [(null? ls0) '()]
      [(memp (cut eq?
                  (car ls0)
                  <>)
             ls1)
       (list-difference eq? (cdr ls0) ls1)]
      [else
       (cons (car ls0)
             (list-difference eq?
                              (cdr ls0)
                              ls1))]))

  )
