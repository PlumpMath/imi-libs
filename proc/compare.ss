#!r6rs

(library (imi proc compare)
  (export make-comparer-by<
          make-comparer-by>
          make-comparer-by</>)
  (import (rnrs))

  ;;; DRAFT

  ; other names?

  (define (make-comparer-by< <)
    (lambda (x y)
      (cond
        [(< x y) '<]
        [(< y x) '>]
        [else '=])))

  (define (make-comparer-by> >)
    (lambda (x y)
      (cond
        [(> x y) '>]
        [(> y x) '<]
        [else '=])))

  (define (make-comparer-by</> < >)
    (lambda (x y)
      (cond
        [(< x y) '<]
        [(> x y) '>]
        [else '=])))

  )
