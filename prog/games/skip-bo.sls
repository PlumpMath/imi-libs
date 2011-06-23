#!r6rs

;(library (imi prog games skip-bo)
;  (export skip-bo?
;          make-stapel
;          make-spieler
;          spieler-arbeitsstapel
;          make-hilfsstapel
;          random-card)
;  (import (rnrs)
;          (srfi :27))

  (define (skip-bo? sth)
    (zero? sth))

  (define (make-stapel n)
    (if (zero? n)
        '()
        (cons (random-card)
              (make-stapel (- n 1)))))

  (define (make-spieler kartenzahl)
    (list (make-stapel kartenzahl)
          (make-stapel 5)
          (make-hilfsstapel)))

  (define (spieler-arbeitsstapel spieler)
    (list-ref spieler 0))

  (define (spieler-handkarten spieler)
    (list-ref spieler 1))

  (define (spieler-hilfsstapel spieler)
    (list-ref spieler 2))

  (define (make-hilfsstapel)
    '(() () () ()))

  (define (make-talon)
    '(() () () ())
    

  (define (hilfsstapel-ref hilfsstapel n)
    (list-ref hilfsstapel n))

  (define (random-card)
    (random-integer 13))

;  )
