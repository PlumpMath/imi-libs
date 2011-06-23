#!r6rs

(library (imi asm arm)
  (export asm)
  (import (rnrs)
          (imi proc dispatcher)
          (imi asm utils)
          (imi asm general)
          (imi asm arm parser conditions)
          (imi asm arm instructions data-processing)
          (imi asm arm instructions branch)
          (imi asm arm instructions memory))



  (define (arm-parser label instr)
    (u32->bv
      (case-match instr
        [,number? () instr]
        [((: instr) (: condition ,condition-instruction?) . (: args))
         (instr condition args)
         (set-condition condition
                        (parse-instr label instr args))]
        [((: instr) . (: args))
         (instr args)
         (parse-instr label instr args)]
        [else
         ()
         (error 'arm-parser
                "invalid instruction"
                instr)])))


  (define asm
    (make-assembler 4 #f arm-parser))





  (define *instructions*
    (append *data-processing-instructions*
            *branch-instructions*
            *memory-instructions*
            ))




  (define (parse-instr label name args)
    (dispatchq name
               *instructions*
               label
               name
               args))

  (define (u32->bv num)
    (let ([bv (make-bytevector 4)])
      (bytevector-uint-set! bv
                            0
                            num
                            (endianness little)
                            4)
      bv))

  )

