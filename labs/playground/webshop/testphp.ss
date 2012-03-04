(import (imi labs playground webshop php))

(define f
  '(define (foo a)
     (define (add x)
       (+ x a))
     (add a)))

(define f*
  '(lambda (x)
     (define (bar a)
       (bar x))
     (bar x)))

(define f1
  '(define (reverse ls)
     (define (null? ls)
       (= 0 ls))
     (define (rev ls r)
       (if (null? ls)
           r
           (rev (- 1 ls)
                (+ ls r))))
     (rev ls 0)))

(define f2
  '(define (foo x)
     (define (loop a)
       (loop x))
     (loop x)))

(define r (extend-frame r.global '(x)))

(maxwidth 40)

#|
(define c (scomp f* #f))
(define b (caddr c))
(define bf (caddr b))
(define bfb (caddr bf))
|#

#;
(parameterize ([maxwidth 40])
  (scomp '(lambda (x) (let ([x (+ x 1)]) (+ x x))) #t)
  (scomp f #t)
  ;(scomp '(define (loop) (loop)) #t)
  ;(scomp '(let ([x 1]) (define (loop a) (loop x))) #t)
  #;
  (scomp '(define (foo a)
            (define (loop b)
              (loop a))
            (loop a))
         #t)
  )
