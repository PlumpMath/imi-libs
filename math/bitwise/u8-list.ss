#!r6rs

(library (imi math bitwise u8-list)
  (export s16->u8-list
          u16->u8-list
          s32->u8-list
          u32->u8-list
          s64->u8-list
          u64->u8-list

          u8-list->s16
          u8-list->u16
          u8-list->s32
          u8-list->u32
          u8-list->s64
          u8-list->u64

          uint->u8-list
          sint->u8-list
          u8-list->uint
          u8-list->sint)
  (import (rnrs)
          (imi sugar cut))

  ;;;; INT->U8-LIST

  (define (set-u8-list bv-set!)
    (lambda (n size)
      (let ([bv (make-bytevector size)])
        (bv-set! bv
                 0
                 n
                 (endianness big)
                 size)
        (bytevector->u8-list bv))))

  (define uint->u8-list (set-u8-list bytevector-uint-set!))
  (define sint->u8-list (set-u8-list bytevector-sint-set!))

  
  ;;;; U8-LIST->INT

  (define (ref-u8-list bv-ref)
    (lambda (ls size)
      (bv-ref (u8-list->bytevector ls)
              0
              (endianness big)
              size)))

  (define u8-list->uint (ref-u8-list bytevector-uint-ref))
  (define u8-list->sint (ref-u8-list bytevector-sint-ref))


  ;;;; SINT->U8-LIST

  (define (sint->u8-list-proc size)
    (cut sint->u8-list <> size))

  (define s16->u8-list (sint->u8-list-proc 2))
  (define s32->u8-list (sint->u8-list-proc 4))
  (define s64->u8-list (sint->u8-list-proc 8))


  ;;;; UINT->U8-LIST

  (define (uint->u8-list-proc size)
    (cut uint->u8-list <> size))

  (define u16->u8-list (uint->u8-list-proc 2))
  (define u32->u8-list (uint->u8-list-proc 4))
  (define u64->u8-list (uint->u8-list-proc 8))


  ;;;; U8-LIST->SINT

  (define (u8-list->sint-proc size)
    (cut u8-list->sint <> size))

  (define u8-list->s16 (u8-list->sint-proc 2))
  (define u8-list->s32 (u8-list->sint-proc 4))
  (define u8-list->s64 (u8-list->sint-proc 8))


  ;;;; U8-LIST->UINT

  (define (u8-list->uint-proc size)
    (cut u8-list->uint <> size))

  (define u8-list->u16 (u8-list->uint-proc 2))
  (define u8-list->u32 (u8-list->uint-proc 4))
  (define u8-list->u64 (u8-list->uint-proc 8))

  )
