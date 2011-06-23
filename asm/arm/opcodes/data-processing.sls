#!r6rs

(library (imi asm arm opcodes data-processing)
  (export data-processing
          
          AND EOR SUB RSB
          ADD ADC SBC RSC
          TST TEQ CMP CMN
          ORR MOV BIC MVN)
  (import (rnrs)
          (prefix (imi math bitwise bit-field) imi:)
          (imi asm arm opcodes addressing-mode1)
          (imi asm arm opcodes conditions)
          (imi asm arm opcodes register))


  ;;;; DATA PROCESSING Instructions
  ;;;
  ;;; Have as every other instruction the cond bits and
  ;;; the shifter operand. The cond field will always be
  ;;; AL and can be changed by passing the value of
  ;;; data-processing to a function from (imi asm arm conditions)
  ;;;
  ;;;  31       28          11                                0
  ;;; +------------+- ... -+------------------------------------+
  ;;; |    cond    |  ...  |          shifter_operand           |
  ;;; +------------+- ... -+------------------------------------+



  ;;;      27 26  25  24       21  20  19       16  15       12
  ;;; ...-+------+---+------------+---+------------+------------+-...
  ;;; ... | 0  0 | I |   opcode   | S |     Rn     |     Rd     | ...
  ;;; ...-+------+---+------------+---+------------+------------+-...
  ;;;
  ;;; The `I` bit is part of the shifter operand and determines,
  ;;; if it is an immediate or a register value. The `S` bit controls,
  ;;; whether the Flags `NZCV` in the `CPSR` register are changed (S=1)
  ;;; or not.
  (define (data-processing opcode s? dest-reg src-reg0 shft-op1)
    (unless (shifter-operand? shft-op1)
      (error 'data-processing
             "not a shifter operand"
             shft-op1))
    (unless (register? src-reg0)
      (error 'data-processing
             "not a register"
             src-reg0))
    (unless (register? dest-reg)
      (error 'data-processing
             "not a register"
             dest-reg))
    (unless (< opcode (expt 2 4))
      (error 'data-processing
             "invalid data-processing opcode"
             opcode))

    (AL (imi:bitwise-copy-bit-field* 0
                                      0 12 shft-op1
                                     12 16 dest-reg
                                     16 20 src-reg0
                                     20 21 (if s? 1 0)
                                     21 25 opcode
                                     25 26 (if (immediate? shft-op1)
                                               1
                                               0)
                                     26 28 #b00)))



  ;;;; THE INSTRUCTION OPCODES

  (define AND #b0000) ;; Logical AND:  Rd := Rn AND shifter_operand
  (define EOR #b0001) ;; Logical Exclusive Or:  Rd := Rn XOR shifter_operand
  (define SUB #b0010) ;; Subtract:  Rd := Rn - shifter_operand
  (define RSB #b0011) ;; Reverse Subtract:  Rd := shifter_operand - Rn
  (define ADD #b0100) ;; Add:  Rd := Rn + shifter_operand
  (define ADC #b0101) ;; Add/Carry:  Rd := Rn + shifter_operand + Carry Flag
  (define SBC #b0110) ;; Subtract/Carry:  Rd := Rn - shifter_operand - NOT(C)
  (define RSC #b0111) ;; Reverse Subtract/Carry:  Rd := shft_op - Rn - NOT(C)
  (define TST #b1000) ;; Test:  Update flags after `Rn AND shifter_operand`
  (define TEQ #b1001) ;; Test Equivalence:  `Rn EOR shifter_operand`
  (define CMP #b1010) ;; Compare:  `Rn - shifter_operand`
  (define CMN #b1011) ;; Compare Negated:  `Rn + shifter_operand`
  (define ORR #b1100) ;; Logical inclusive Or:  Rd := Rn IOR shifter_operand
  (define MOV #b1101) ;; Move:  Rd := shifter_operand
  (define BIC #b1110) ;; Bit Clear:  Rd := Rn AND NOT(shifter_operand)
  (define MVN #b1111) ;; Move Not/Negated:  Rd := NOT(shifter_operand)


  )

