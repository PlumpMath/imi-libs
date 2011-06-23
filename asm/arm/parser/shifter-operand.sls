#!r6rs

(library (imi asm arm parser shifter-operand)
  (export parse-shifter-operand)
  (import (rnrs)
          (imi asm utils)
          (imi proc dispatcher)
          (imi asm arm parser register)
          (imi asm arm opcodes addressing-mode1))


  ;;; parses all different shifter operand
  ;;; forms, which can be
  ;;;   immediate    - a number
  ;;;   label        - a label
  ;;;   R0/R1/...    - a register
  ;;;   (SHIFT ...)  - a special shift (see below)
  (define (parse-shifter-operand label operand)
    (case-match operand
      [(: immediate-operand ,number?)
       (immediate-operand)
       (create-immediate immediate-operand)]
      [(: reg ,get-register)
       (reg)
       (register (get-register reg))]
      [(: lbl ,symbol?)
       (lbl)
       (create-immediate (label lbl))]
      [(: shift ,list?)
       (shift)
       (parse-shift shift)]
      [else
       ()
       (error 'parse-shifter-operand
              "invalid shifter operand"
              operand)]))


  ;;; parses the different register
  ;;; shifts, see below
  (define (parse-shift operand)
    (dispatchq (car operand)
               *shifter*
               (car operand)
               (cdr operand)))




  ;;; creates an evaluator for one special shift
  ;;; operation, which can distinguish the
  ;;;   (SHIFT Register shift)
  ;;; forms, where SHIFT is a given shift
  ;;; operand, Register is a Register (R0 ...)
  ;;; and shift is either a register or an
  ;;; immediate
  (define (make-shifter normal with-register)
    (lambda (name args)
      (case-match args
        [((: reg) (: immediate ,number?))
         (reg immediate)
         (check reg get-register name "invalid shift: invalid register")
         (normal immediate (get-register reg))]
        [((: reg) (: shift-reg))
         (reg shift-reg)
         (check reg get-register name "invalid shift: invalid register")
         (check shift-reg get-register name 
                "invalid shift: not a number or register")
         (with-register shift-reg reg)]
        [else () (error name "wrong argument count" args)])))




  ;;; this is the special evaluator for a RRX
  ;;; (rotate right with extend) shift, which
  ;;; can only rotate one position a time, so
  ;;; it has no shift argument and is of the
  ;;; form
  ;;;   (RRX Register)
  (define (rrx-shifter name args)
    (case-match args
      [((: reg))
       (reg)
       (check reg get-register name "invalid register")
       (register/rotate-right/extend
         (get-register reg))]
      [else () (error name "wrong argument count" args)]))




  ;;; the shifter procedures, which shift a
  ;;; register by a given amount, an immediate
  (define *shifter-procs*
    (list register/logical-shift-left
          register/logical-shift-right
          register/arithmetical-shift-right
          register/rotate-right))

  ;;; the shifter procedures, which shift a
  ;;; register by a given amount of another
  ;;; register
  (define *shifter/reg-procs*
    (list register/logical-shift-left/register
          register/logical-shift-right/register
          register/arithmetical-shift-right/register
          register/rotate-right/register))


  ;;; the shifter-alist, which associates a
  ;;; shifter name (LSL LSR ASR ROR RRX)
  ;;; with its dispatcher
  (define *shifter*
    (let* ([shifter-names '(LSL LSR ASR ROR)]
           [shifter-procs (map make-shifter
                               *shifter-procs*
                               *shifter/reg-procs*)])
      (cons (cons 'RRX rrx-shifter)
            (map cons shifter-names shifter-procs))))


  )

