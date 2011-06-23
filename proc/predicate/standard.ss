#!r6rs

(library (imi proc predicate standard)
  (export any?
          false?
          cons/p
          list/p
          list*/p
          listof/p
          vector/p
          vectorof/p)
  (import (rnrs)
          (imi proc predicate check)
          (imi proc predicate logic)
          (imi utils math curried-operator))

  ;;; matches any value
  (define any?
    (lambda (x) #t))

  ;;; matches false
  (define false?
    (lambda (x) (not x)))

  ;;; pair/cons predicate
  (define (cons/p car-pred cdr-pred)
    (lambda (x)
      (and (pair? x)
           (check-predicate (car x) car-pred)
           (check-predicate (cdr x) cdr-pred))))

  ;;; list predicate, which matches
  ;;;  exact length and args
  (define (list/p . preds)
    (if (null? preds)
      null?
      (cons/p (car preds)
              (apply list/p (cdr preds)))))

  ;;; list predicate, which matches
  ;;;  all preds and the cdr of the
  ;;;  rest with the last pred
  (define (list*/p pred . preds)
    (if (null? preds)
      pred
      (cons/p pred
              (apply list*/p (cdr preds)))))

  ;;; list predicate for a list with
  ;;;   a variable type, but only with
  ;;;   a specific value type in it
  (define (listof/p pred)
    (or/p null?
          (cons/p pred (listof/p pred))))

  ;;; vector predicate which matches
  ;;;  exact length and args
  (define (vector/p . preds)
    (lambda (x)
      (and (vector? x)
           (let loop ([i 0] [preds preds])
             (if (null? preds)
               (= i (vector-length x))
               (and (< i (vector-length x))
                    (check-predicate
                      (vector-ref x i)
                      (car preds))
                    (loop (add1 i)
                          (cdr preds))))))))

  ;;; vector predicate which matches
  ;;;  all elements of vector to `pred`
  (define (vectorof/p pred)
    (lambda (x)
      (and (vector? x)
           (let loop ([i 0])
             (or (= i (vector-length x))
                 (and (check-predicate
                        (vector-ref x i)
                        pred)
                      (loop (add1 i))))))))

  )
