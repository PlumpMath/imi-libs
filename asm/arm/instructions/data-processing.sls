#!r6rs

(library (imi asm arm instructions data-processing)
  (export *data-processing-instructions*)
  (import (rnrs)
          (imi sugar rec)
          (imi asm utils)
          (imi asm arm opcodes data-processing)
          (imi asm arm parser register)
          (imi asm arm parser shifter-operand))


  ;;; OPCODES and NAMES
  
  ;;; for the instructions which work only
  ;;; with Rd and shifter_operand
  (define *opcode1*
    (list MOV MVN))
  (define *name1*
    '(MOV MVN))

  ;;; for the instructions which work only
  ;;; with Rn and shifter_operand
  (define *opcode2*
    (list CMP CMN TST TEQ))
  (define *name2*
    '(CMP CMN TST TEQ))

  ;;; for the instructions which work with
  ;;; all operands Rd, Rn and shifter_operand
  (define *opcode3*
    (list ADD SUB RSB ADC SBC RSC AND BIC EOR ORR))
  (define *name3*
    '(ADD SUB RSB ADC SBC RSC AND BIC EOR ORR))



  ;;; all data processing mnemonics
  (define *all-names*
    (append *name1* *name2* *name3*))




  ;;; creates the opcode1 instruction procedure
  ;;; (which means with only Rd and shifter_operand
  ;;; as arguments)
  (define (make-opcode1-instruction opcode)
    (lambda (label name args)
      (define (create-opcode s? dest-reg shft-op)
        (check-register dest-reg "invalid register"
          (lambda (dest-reg)
            (data-processing opcode
                             s?
                             dest-reg
                             0
                             (parse-shifter-operand label shft-op)))))

      (case-match args
        [((: dest-reg) (: shft-op))
         (dest-reg shft-op)
         (create-opcode #f dest-reg shft-op)]
        [((: s?) (: dest-reg) (: shft-op))
         (s? dest-reg shft-op)
         (create-opcode s? dest-reg shft-op)]
        [else () (error name "invalid argument count" args)])))


  ;;; the final opcode1 instruction parser list
  (define *opcode1-instructions*
    (let ([instructions (map make-opcode1-instruction *opcode1*)])
      (map cons *name1* instructions)))



  ;;; creates the opcode2 instruction procedure
  ;;; (which means with only Rn and shifter_operand
  ;;; as arguments)
  (define (make-opcode2-instruction opcode)
    (lambda (label name args)
      (case-match args
        [((: reg) (: operand))
         (reg operand)
         (check-register reg "invalid register"
           (lambda (reg)
             (data-processing opcode
                              #t
                              0
                              reg
                              (parse-shifter-operand label operand))))]
        [else
         ()
         (error name
                "wrong argument count"
                args)])))


  ;;; the final opcode2 instruction parser list
  (define *opcode2-instructions*
    (let ([instructions (map make-opcode2-instruction *opcode2*)])
      (map cons *name2* instructions)))




  ;;; creates the opcode3 instruction procedure
  ;;; (which means with all arguments)
  (define (make-opcode3-instruction opcode)
    (lambda (label name args)
      (define (create-opcode s? dest-reg reg0 shft-op)
        (check-register (list dest-reg reg0)
                        '("invalid destination register"
                          "invalid source register")
          (lambda (dest-reg reg0)
            (data-processing opcode
                             s?
                             dest-reg
                             reg0
                             (parse-shifter-operand label shft-op)))))

      (case-match args
        [((: dest-reg) (: reg0) (: shft-op))
         (dest-reg reg0 shft-op)
         (create-opcode #f dest-reg reg0 shft-op)]
        [((: s?) (: dest-reg) (: reg0) (: shft-op))
         (s? dest-reg reg0 shft-op)
         (create-opcode s? dest-reg reg0 shft-op)]
        [else () (error name "invalid argument count" args)])))


  ;;; the final opcode3 instruction parser list
  (define *opcode3-instructions*
    (let ([instructions (map make-opcode3-instruction *opcode3*)])
      (map cons *name3* instructions)))






  ;;; all data instructions as alist with
  ;;; name and procedure, which can further
  ;;; dispatch the instruction
  (define *data-processing-instructions*
    (append *opcode1-instructions*
            *opcode2-instructions*
            *opcode3-instructions*))






  (define (check-register reg errormsg proc)
    (define who 'check-register)
    (cond
      [(get-register reg)
       => proc]
      [else
       (error who errormsg reg)]))

  (define (check-registers regs errormsgs proc)
    (define who 'check-registers)
    (let ([regs (map (lambda (reg errormsg)
                       (cond
                         [(get-register reg)]
                         [else (error who errormsg reg)]))
                     regs
                     errormsgs)])
      (apply proc regs)))

  )

