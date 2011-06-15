(library (imi random list)
  (export shuffle
          random-choice)
  (import (rnrs)
          (imi random number)
          (imi random proc))

  ;;;;;;;;;;;;;;;; TODO ;;;;;;;;;;;;;;;;

  ;;; shuffles the contents of `ls`
  ;;;
  ;;; ls - list?
  ;;;  -> list?
  (define (shuffle ls)
    (list-sort
      (lambda (x y)
        (random-boolean))
      ls))

  ;;; returns a random element of `ls`
  ;;;
  ;;; ls - (listof/c elem)
  ;;;  -> elem
  (define (random-choice ls)
    (list-ref ls
              (random-integer
                (length ls))))

  )
