#!r6rs

(library (imi math bitwise bit-field)
  (export bitwise-mask
          bitwise-mask*
          bitwise-mask**
          bitwise-bit-field
          bitwise-copy-bit-field
          bitwise-copy-bit-field*
          bitwise-rotate-bit-field
          bitwise-reverse-bit-field) ;TODO
  (import (except (rnrs)
                  bitwise-bit-field
                  bitwise-copy-bit-field
                  bitwise-rotate-bit-field
                  bitwise-reverse-bit-field))


  ;;;; Bitwise BIT-FIELD
  ;;;
  ;;; Warning: As you can see, this library shadows some definitions
  ;;;          of R6RS, so you have to check in most cases that you
  ;;;          don't import two different bindings with the same name.
  ;;;
  ;;; When?   If you import the whole (rnrs) library or (rnrs bitwise)
  ;;;         and similar
  ;;; How?    Just give one library a `prefix` or `rename` the bindings
  ;;;         or delimit the imported bindings by either `except` or `only`
  ;;;         See the R6RS Library Forms documentation
  ;;;
  ;;; Why?    Why this shadows the already available standard bindings?
  ;;;         Well, I was too lazy to change something or dig into the
  ;;;         ikarus implementation, as it doesn't have an implementation
  ;;;         of some bit-field procedures.  So the easiest and fastest
  ;;;         way was to implement them by myself and provide them in my
  ;;;         set of Scheme libraries.
  ;;;
  ;;;          - Daniel Krueger
  ;;;
  ;;;         PS: Yes, this library also has some useful /new/ bindings
  ;;;             which are /not/ in the R6RS standard.
  ;;;         PPS: And by now it also lacks the implementation of one binding,
  ;;;              bitwise-bit-field-reverse :D  [(sarcasm "] I know, I've
  ;;;              done this much better than ikarus has [")] :D

  (define (bitwise-mask len)
    (- (bitwise-arithmetic-shift-left 1 len)
       1))

  (define (bitwise-mask* from to)
    (bitwise-xor
      (bitwise-mask to)
      (bitwise-mask from)))

  (define bitwise-mask**
    (case-lambda
      [() 0]
      [(from to . rest)
       (bitwise-ior
         (bitwise-mask* from to)
         (apply bitwise-mask** rest))]))

  (define (bitwise-bit-field num start end)
    (let ([mask (bitwise-not
                  (bitwise-arithmetic-shift-left -1 end))])
      (bitwise-arithmetic-shift-right
        (bitwise-and num mask)
        start)))

  (define (bitwise-copy-bit-field field start end num)
    (let ([overwrite-mask (bitwise-mask* start end)])
      (bitwise-if overwrite-mask
                  (bitwise-arithmetic-shift-left num start)
                  field)))

  (define bitwise-copy-bit-field*
    (case-lambda
      [(field start end num)
       (bitwise-copy-bit-field field start end num)]
      [(field start end num . rest)
       (apply bitwise-copy-bit-field*
              (bitwise-copy-bit-field field start end num)
              rest)]))

  (define (bitwise-rotate-bit-field field start end count)
    (let* ([field-to-rotate (bitwise-bit-field field start end)]
           [width (- end start)]
           [count (mod count width)])
      (if (positive? width)
          (bitwise-copy-bit-field
            field
            start
            end
            (bitwise-ior
              (bitwise-arithmetic-shift-left field-to-rotate count)
              (bitwise-arithmetic-shift-right field-to-rotate (- width count))))
          field)))

  (define (bitwise-reverse-bit-field field start end)
    ;;;TODO
    field)

  )

