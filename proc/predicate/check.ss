#!r6rs

(library (imi proc predicate check)
  (export check-predicate
          verify-args)
  (import (rnrs))

  ;;; checks if `x` applies to the predicate
  ;;;
  ;;; x - any?
  ;;; predicate - (-> any? any?)
  ;;;  -> any?
  (define (check-predicate x predicate)
    (predicate x))

  ;;; checks if the args have all the correct
  ;;;  type (by predicate-checking), raises
  ;;;  an assertion-violation with a given
  ;;;  message and the wrong arg as irritant
  ;;;
  ;;; who - symbol?
  ;;; arg - any?
  ;;; msg - string?
  ;;; pred - (-> any? any?)
  ;;; rest - (rec resttype
  ;;;             (or/c null?
  ;;;                   (list*/c any? string? (-> any? any?)
  ;;;                            resttype)))
  ;;;  -> void?
  (define (verify-args who arg msg pred . rest)
    (if (check-predicate arg pred)
      (unless (null? rest)
        (apply verify-args who rest))
      (raise
        (condition
          (make-who-condition who)
          (make-assertion-violation)
          (make-message-condition msg)
          (make-irritants-condition (list arg))))))

  )
