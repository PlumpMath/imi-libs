(library (imi foreign utils)
  (export sizeof
          c-symbol->scm-symbol
          string->c-char*
          c-char*->string)
  (import (rnrs)
          (imi math)
          (ikarus foreign))

  (define (sizeof type)
    (case type
      [(char schar uchar)    1]
      [(short sshort ushort) 2]
      [(int sint uint)       4]
      [(long slong ulong) (pointer-size)]
      [(float)               4]
      [(double)              8]
      [(pointer)          (pointer-size)]
      [else (error 'sizeof "unkown type" type)]))

  (define (c-symbol->scm-symbol sym)
    (string->symbol
      (list->string
        (map (lambda (char)
               (case char
                 [(#\_) #\-]
                 [else char]))
             (string->list
               (symbol->string
                 sym))))))

  (define (string->c-char* str)
    (let* ([utf8 (string->utf8 str)]
           [len (bytevector-length utf8)]
           [char* (malloc len)])
      (let loop ([i 0])
        (when (< i len)
          (pointer-set-c-char! char* i
                               (bytevector-s8-ref utf8 i))
          (loop (add1 i))))
      char*))

  (define (c-char*->string char*)
    (utf8->string
      (u8-list->bytevector
        (let loop ([i 0])
          (let ([c (pointer-ref-c-signed-char char* i)])
            (cond
              [(zero? c) '()]
              [else 
                (cons c 
                      (loop (add1 i)))]))))))

  )
