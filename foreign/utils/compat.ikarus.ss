(library (imi foreign utils compat)
  (export get-getter
          get-setter)
  (import (rnrs)
          (ikarus foreign))

  (define type-getter
    `(((char schar) ,pointer-ref-c-signed-char)
      ((uchar)      ,pointer-ref-c-unsigned-char)
      ((short sshort) ,pointer-ref-c-signed-short)
      ((ushort)       ,pointer-ref-c-unsigned-short)
      ((int sint)   ,pointer-ref-c-signed-int)
      ((uint)       ,pointer-ref-c-unsigned-int)
      ((long slong) ,pointer-ref-c-signed-long)
      ((ulong)      ,pointer-ref-c-unsigned-long)
      ((float)      ,pointer-ref-c-float)
      ((double)     ,pointer-ref-c-double)
      ((pointer)    ,pointer-ref-c-pointer)))

  (define (get-getter who type)
    (cadr (or (assp (lambda (types)
                      (memq type types))
                    type-getter)
              (error who "unknown c-type" type))))

  (define type-setter
    `(((char schar uchar)    ,pointer-set-c-char!)
      ((short sshort ushort) ,pointer-set-c-short!)
      ((int sint uint)       ,pointer-set-c-int!)
      ((long slong ulong)    ,pointer-set-c-long!)
      ((float)   ,pointer-set-c-float!)
      ((double)  ,pointer-set-c-double!)
      ((pointer) ,pointer-set-c-pointer!)))

  (define (get-setter who type)
    (cadr (or (assp (lambda (types)
                      (memq type types))
                    type-setter)
              (error who "unkonwn c-type" type))))

  )
