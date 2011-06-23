#!r6rs

(library (imi type pred logic)
  (export or/p
          and/p)
  (import (rnrs)
          (imi type pred))

  ;;; checks if one pred applies in order;
  ;;;  like or
  (define-syntax or/p
    (syntax-pred x (pred ...)
      (or (check-pred pred x) ...)))

  ;;; checks if all preds apply in order;
  ;;;  like and
  (define-syntax and/p
    (syntax-pred x (pred ...)
      (and (check-pred pred x) ...)))

  )
