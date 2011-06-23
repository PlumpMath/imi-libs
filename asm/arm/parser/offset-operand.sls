#!r6rs

(library (imi asm arm parser offset-operand)
  (export parse-offset-operand)
  (import (rnrs)
          (imi asm utils)
          (imi asm arm parser register)
          (imi asm arm opcodes addressing-mode2))


  ;;; parses an offset operand which can be of the
  ;;; forms
  ;;;   immediate    - a constant number as offset
  ;;;   R0/R1/...    - a register as offset
  ;;;   (SHIFT R0/R1/... ...)  - a register with a
  ;;;                            specific shift
  (define (parse-offset-operand label addr-mode sign operand)
    (case-match operand
      [(: immediate ,number?)
       (immediate)
       (parse-immediate-offset addr-mode
                               sign
                               operand)]
      [(: reg ,(get-register operand))
       (reg)
       (parse-register-offset sign
                              addr-mode
                              (get-register reg))]
      [(: lbl ,symbol?)
       (lbl)
       (parse-immediate-offset addr-mode
                               '+
                               (label lbl))]
      [(: shifted ,list?)
       (shifted)
       (parse-scaled-register-offset sign
                                     addr-mode
                                     shifted)]
      [else
       ()
       (error 'dispatch-offset-operand
              "invalid offset operand"
              operand)]))


  ;;; parses the immediate offset
  (define (parse-immediate-offset addr-mode sign operand)
    (immediate-offset addr-mode (parse-sign sign operand)))

  ;;; parses the register offset
  (define (parse-register-offset sign addr-mode reg)
    (register-offset (eq? sign '+)
                     addr-mode
                     reg))

  ;;; parses the shifted register offset
  (define (parse-scaled-register-offset sign addr-mode operand)
    (let ([shift-mode (car operand)]
          [reg (or (get-register (cadr operand))
                   (error 'parse-scaled-register-offset
                          "invalid register"
                          (cadr operand)))]
          [shift (caddr operand)])
      (scaled-register-offset (eq? sign '+)
                              addr-mode
                              shift-mode
                              shift
                              reg)))


  ;;; applies the given sign to the operand
  (define (parse-sign sign operand)
    (case sign
      [(+) operand]
      [(-) (- operand)]
      [else (error 'parse-sign
                   "invalid sign"
                   sign)]))




  )

