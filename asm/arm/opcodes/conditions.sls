#!r6rs

(library (imi asm arm opcodes conditions)
  (export EQ ZS   NE ZC
          HS CS   LO CC
          MI NS   PL NC
             VS      VC
             
          HI      LS
          GE      LT
          GT      LE
          AL      NV ;NV is RESERVED!
          )
  (import (rnrs)
          (prefix (imi math bitwise bit-field)
                  imi:)
          (imi asm arm opcodes cond))

  (define (instr-cond code)
    (lambda (instr)
      (when (= (imi:bitwise-bit-field instr 28 32)
               #b1111)
        (error 'condition-overwrite
               "unconditional instruction can not be changed"
               instr))
      (imi:bitwise-copy-bit-field
        code
        0 28 instr)))


  (define EQ (instr-cond equal))        (define ZS (instr-cond zero-set))
  (define NE (instr-cond not-equal))    (define ZC (instr-cond zero-clear))
  (define HS (instr-cond higher/same))  (define CS (instr-cond carry-set))
  (define LO (instr-cond lower))        (define CC (instr-cond carry-clear))
  (define MI (instr-cond minus))        (define NS (instr-cond negative-set))
  (define PL (instr-cond plus))         (define NC (instr-cond negative-clear))
                                        (define VS (instr-cond overflow-set))
                                        (define VC (instr-cond overflow-clear))

  (define HI (instr-cond higher))
  (define LS (instr-cond lower/same))
  (define GE (instr-cond greater/equal))
  (define LT (instr-cond less))
  (define GT (instr-cond greater))
  (define LE (instr-cond less/equal))
  (define AL (instr-cond always))
  (define NV (instr-cond never)) ;RESERVED - do not use!

  )

