(library (imi time convert)
  (export week
          day
          hour
          minute
          second
          millisecond
          nanosecond)
  (import (rnrs))

  (define (week x)
    (day (* 7 x)))

  (define (day x)
    (hour (* 24 x)))

  (define (hour x)
    (minute (* 60 x)))

  (define (minute x)
    (second (* 60 x)))

  (define (second x)
    x)

  (define (millisecond x)
    (second (/ x 1000)))

  (define (nanosecond x)
    (millisecond (/ x 1000)))

  )
