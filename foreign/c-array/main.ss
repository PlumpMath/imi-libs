(library (imi foreign c-array)
  (export make-c-array)
  (import (rnrs)
          (imi foreign utils)
          (imi foreign utils compat))

  (define (make-c-array type ptr)
    (let ([elemsize (sizeof type)]
          [ref (get-getter 'c-array type)])
      (lambda (n)
        (ref ptr (* elemsize n)))))

  )
