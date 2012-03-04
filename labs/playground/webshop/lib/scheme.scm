(define-alias isnull function "isnull")

(define-alias reduce function "array_reduce")
(define-alias slice  function "array_slice")

(define-operator and "and")
(define-operator or "or")

(define-class (Pair)
  (public car)
  (public cdr)
  
  (public (__constructor a d)
    (set! (slot this car) a)
    (set! (slot this cdr) d)
    this)
  )

(define (pair-list? x)
  (or (instanceof x Pair)
      (isnull x)))

(define ($cons a d)
  (make Pair a d))

(define ($car p)
  (slot p car))

(define ($cdr p)
  (slot p cdr))

(define (array->list arr)
  (reduce arr
          (lambda (ls elem)
            ($cons elem ls))
          '()))

(define (cons a d)
  (make Pair a (if (pair-list? d)
                   d
                   (array->list d))))

(define-syntax car
  [(car (app ...))
   (let ([/x/ (app ...)])
     (car /x/))]
  [(car p)
   (if (instanceof p Pair)
       (slot p car)
       (vector-ref p 0))])

(define-syntax cdr
  [(cdr p)
   (if (instanceof p Pair)
       (slot p cdr)
       (array->list (slice p 1)))])

(car (cons 1 2))
