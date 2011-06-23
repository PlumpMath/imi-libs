#!r6rs

(library (imi type pred standard)
  (export cons/p

          listof/p
          list/p
          list*/p
          )
  (import (rnrs)
          (imi type pred)
          (imi type pred logic))

  (define cons/p
    (pred x (pred-car pred-cdr)
      (and (pair? x)
           (check-predicate pred-car x)
           (check-predicate pred-cdr x))))

  )
