#!r6rs

(library (imi math mapping)
  (export mapping-linear
          mapping-around
          mapping-around-percent
          ;mapping-logarithmic
          )
  (import (rnrs))

  ;;; maps the range 0 to 1 to the
  ;;;  specified range from `from`
  ;;;  to `to` linear; useful for
  ;;;  getting a specific range of
  ;;;  numbers from a real random
  ;;;  source
  ;;;
  ;;; from - number?
  ;;; to - number?
  ;;;  -> (-> number?
  ;;;         (number-between/c 0 1))
  (define (mapping-linear from to)
    (let ([range (- to from)])
      (lambda (x)
        (+ (* x range)
           from))))

  ;;; maps 0 to 1 to the range `range`
  ;;;  around `n`, same as mapping it
  ;;;  with mapping-linear from
  ;;;  `(- n range)` to `(+ n range)`
  ;;;
  ;;; n - number?
  ;;; range - number?
  ;;;  -> (-> number?
  ;;;         (number-between/c 0 1))
  (define (mapping-around n range)
    (mapping-linear (- n range)
                    (+ n range)))

  ;;; maps 0 to 1 `(* percentual-range n)`
  ;;;  around `n`
  ;;;
  ;;; n - number?
  ;;; percentual-range -
  ;;;    (number-between/c 0 1)
  ;;;  -> (-> number?
  ;;;         (number-between/c 0 1))
  (define (mapping-around-percent n percentual-range)
    (mapping-around n (* n percentual-range)))

  )
