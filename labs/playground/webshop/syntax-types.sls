#!r6rs

(library (imi labs playground webshop syntax-types)
  (export $cons $list $car $cdr $pair?
          $symbol $symbol? $symbol=?
          )
  (import (rnrs))

  (define ($cons a d)
    (cons a d))

  (define $list list)

  (define ($car p)
    (car p))

  (define ($cdr p)
    (cdr p))

  (define ($pair? p)
    (pair? p))

  (define ($symbol s)
    s)

  (define ($symbol? s)
    (symbol? s))

  (define ($symbol=? s1 s2)
    (symbol=? s1 s2))

  )
