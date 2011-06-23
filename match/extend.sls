#!r6rs

(library (imi match extend)
  (export matcher-extend
          matcher-extend*)
  (import (rnrs))

  ;;; It creates a matcher without the second argument,
  ;;; behaves in all other ways just like `matcher-extend*`
  (define (matcher-extend basic-matcher . extensions)
    (let ([matcher (apply matcher-extend*
                          basic-matcher
                          extensions)])
      (lambda (pattern)
        (matcher pattern matcher))))

  ;;; extends a given `basic-matcher` (which must be
  ;;; of the star-form, that means take two arguments
  ;;; where the second argument is the matcher to call
  ;;; recursively) by the extensions given.
  ;;; The extensions have to be procedures with three
  ;;; arguments: the pattern, basic-matcher and matcher
  ;;; they should produce a one-argument procedure which
  ;;; matches the given pattern to its input, just like
  ;;; basic-matcher (see (imi match basic))
  (define matcher-extend*
    (case-lambda
      [(basic-matcher extension)
       (lambda (pattern matcher)
         (extension pattern basic-matcher matcher))]
      [(basic-matcher extension . rest)
       (apply matcher-extend*
              (matcher-extend* basic-matcher extension)
              rest)]))


  )

