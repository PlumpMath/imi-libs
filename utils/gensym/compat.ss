#!r6rs

(library (imi utils gensym compat)
  (export gensym)
  (import (rnrs)
          (imi string format)
          (imi math)
          (imi sugar)
          (imi sugar begin0))

  (define gensym
    (let ([counter 0])
      (lambda ()
        (begin0
          (string->symbol
            (format "gensym-~s" counter))
          (update! counter add1)))))

  )
