#!r6rs

(library (imi asm arm opcodes addressing-mode1)
  (export shifter-operand?   immediate?
          
          immediate-value    create-immediate
          register
          
          register/logical-shift-left
          register/logical-shift-left/register
          register/logical-shift-right
          register/logical-shift-right/register
          
          register/arithmetical-shift-right
          register/arithmetical-shift-right/register
          
          register/rotate-right
          register/rotate-right/register
          register/rotate-right/extend)
  (import (rnrs)
          (imi asm arm opcodes register)
          (prefix (imi math bitwise bit-field) imi:))


  ;;;; SHIFTER OPERANDS
  ;;;
  ;;; This are the operand encodings for data processing
  ;;; instructions.
  ;;; ~ are all encoded in one chunk, except for immediate,
  ;;; which has an immediate flag/bit `I` set at bit 25 ...
  ;;; This bit must be in all other operands clear, but
  ;;; I will leave it out for simplification.
  ;;;
  ;;;       25          11                                0
  ;;; ... -+---+- ... -+------------------------------------+
  ;;; ...  | I |  ...  |          shifter_operand           |
  ;;; ... -+---+- ... -+------------------------------------+
  

  ;;; checks if the given argument is a valid shifter operand
  (define (shifter-operand? x)
    (zero? (bitwise-and x
                        (bitwise-not
                          (imi:bitwise-mask**  0 12
                                              25 26)))))

  #;
  (define addressing-mode1-opcode
    `((25   (11          0))
      ( I   shifter-operand)
      (#f          #f      )))

  #;
  (define (make-immediate-operand desc)
    (opcode-fill-in* addressing-mode1-opcode
                     'I 1
                     'shifter-operand desc))

  #;
  (define (make-register-operand desc)
    (opcode-fill-in* addressing-mode1-opcode
                     'I 0
                     'shifter-operand desc))


  ;;;; IMMEDIATE


  ;;;       25          11        8   7                    0
  ;;; ... -+---+- ... -+------------+------------------------+
  ;;; ...  | 1 |  ...  | rotate_imm |        immed_8         |
  ;;; ... -+---+- ... -+------------+------------------------+
  ;;;
  ;;; result: `immed_8` rotated by `(* rot_imm 2)`
  ;;;
  ;;; the immediate value is defined by an
  ;;;   even rotate which is encoded in 4 bits
  ;;;   without the first bit and a 8 bit
  ;;;   immediate which is rotated
  (define (immediate-value rotate immed)
    (unless (and (even? rotate)
                 (< (/ rotate 2)
                    (expt 2 4)))
      (error 'immediate-value
             "rotate has to be an even integer less or equal 30"
             rotate))
    (unless (< immed (expt 2 8))
      (error 'immediate-value
             "immed has to be an integer less than 256 (2^8)"
             immed))

    (imi:bitwise-copy-bit-field* 0
                                  0  8 immed
                                  8 12 (/ rotate 2)
                                 25 26 1))

  #;
  (define (rotate-imm rot)
    (/ rot 2))

  #;
  (define (valid-rotate? rot)
    (even? rot))

  #;
  (define immediate-value-opcode
    (make-immediate-operand
      `(((11                   8)   (7   0))
        ((rotate-imm ,rotate-imm)   immed-8)
        (   ,valid-rotate-imm?         #f  ))))


  ;;; finds a representation for an immediate
  ;;;   itself or, if impossible, returns #f
  (define (create-immediate val)
    (let loop ([shift 0]
               [val val])
      (cond
        [(< val 256)
         (immediate-value shift val)]
        [(> shift 30) #f]
        [else
         (loop (+ 2 shift)
               (rotate-right32 val 2))])))

  (define (rotate-right32 num width)
    (imi:bitwise-rotate-bit-field num 0 32 (- 32 width)))


  ;;; checks if a shifter operand is an immediate
  ;;; (by checking the 25th bit)
  (define (immediate? operand)
    (bitwise-bit-set? operand 25))




  ;;;  11 10  9  8  7   6  5  4   3        0
  ;;; +---------------+---------+------------+
  ;;; | 0  0  0  0  0 | 0  0  0 |     Rm     |
  ;;; +---------------+---------+------------+
  ;;;
  ;;; result: only the register `Rm`
  ;;;
  ;;; this is the opcode to get only the register
  (define (register reg)
    (unless (register? reg)
      (error 'register
             "this is not an register"
             reg))
    reg)






  ;;;; LOGICAL SHIFTS
  
  ;;;;; Left


  ;;;  11           7   6  5  4   3        0
  ;;; +---------------+---------+------------+
  ;;; |   shift_imm   | 0  0  0 |     Rm     |
  ;;; +---------------+---------+------------+
  ;;;
  ;;; result: contents of `Rm` logically shifted
  ;;;         left by `shift_imm`
  (define (register/logical-shift-left shift reg)
    (unless (< shift 32)
      (error 'register/logical-shift-left
             "shift has to be less than 32"
             shift))
    (unless (register? reg)
      (error 'register/logical-shift-left
             "not a valid register"
             reg))
    (imi:bitwise-copy-bit-field* 0
                                 0  4 reg
                                 4  7 #b000
                                 7 12 shift))

  #;
  (define register/logical-shift-left-opcode
    (make-register-operand
      `(((11    7)   (6 5 4)   (3  0))
        (shift-imm   (0 0 0)     Rm  )
        (    #f         #f       #f  ))))




  ;;;  11        8   7  6  5  4   3        0
  ;;; +------------+------------+------------+
  ;;; |     Rs     | 0  0  0  1 |     Rm     |
  ;;; +------------+------------+------------+
  ;;;
  ;;; result: contents of `Rm` logically shifted
  ;;;         by amount given in `Rs`
  (define (register/logical-shift-left/register shift-reg reg)
    (unless (and (register? shift-reg)
                 (register? reg))
      (error 'register/logical-shift-left/register
             "both arguments have to be registers"
             shift-reg reg))
    (imi:bitwise-copy-bit-field* 0
                                 0  4 reg
                                 4  8 #b0001
                                 8 12 shift-reg))

  #;
  (define register/logical-shift-left/register-opcode
    (make-register-operand
      `(((11 8)   (7 6 5 4)   (3 0))
        (  Rs     (0 0 0 1)     Rm )
        (  #f         #f        #f ))))


  ;;;;; Right


  ;;;  11           7   6  5  4   3        0
  ;;; +---------------+---------+------------+
  ;;; |   shift_imm   | 0  1  0 |     Rm     |
  ;;; +---------------+---------+------------+
  ;;;
  ;;; result: contents of `Rm` shifted logically
  ;;;         right by `shift_imm`
  (define (register/logical-shift-right shift reg)
    (unless (<= 1 shift 32)
      (error 'register/logical-shift-right
             "shift has to be a value between 1 and 32 inclusive"
             shift))
    (unless (register? reg)
      (error 'register/logical-shift-right
             "not a valid register"
             reg))
    (imi:bitwise-copy-bit-field* 0
                                 0  4 reg
                                 4  7 #b010
                                 7 12 (if (= shift 32)
                                          0
                                          shift)))

  #;
  (define register/logical-shift-right-opcode
    (make-register-operand
      `(((11    7)   (6 5 4)   (3 0))
        (shift-imm   (0 1 0)     Rm )
        (    #f         #f       #f ))))




  ;;;  11        8   7  6  5  4   3        0
  ;;; +------------+------------+------------+
  ;;; |     Rs     | 0  0  1  1 |     Rm     |
  ;;; +------------+------------+------------+
  ;;;
  ;;; result: contents of `Rm` logically shifted
  ;;;         right by amount given in `Rs`
  (define (register/logical-shift-right/register shift-reg reg)
    (unless (and (register? shift-reg)
                 (register? reg))
      (error 'register/logical-shift-right/register
             "both arguments have to be registers"
             shift-reg reg))
    (imi:bitwise-copy-bit-field* 0
                                 0  4 reg
                                 4  8 #b0011
                                 8 12 shift-reg))

  #;
  (define register/logical-shift-right/register-opcode
    (make-register-operand
      `(((11 8)   (7 6 5 4)   (3 0))
        (  Rs     (0 0 1 1)     Rm )
        (  #f         #f        #f ))))




  ;;;; ARITHMETIC SHIFTS


  ;;;  11           7   6  5  4   3        0
  ;;; +---------------+---------+------------+
  ;;; |   shift_imm   | 1  0  0 |     Rm     |
  ;;; +---------------+---------+------------+
  ;;;
  ;;; result: contents of `Rm` arithmetically
  ;;;         shifted right by `shift_imm`
  (define (register/arithmetical-shift-right shift reg)
    (unless (<= 1 shift 32)
      (error 'register/arithmetical-shift-right
             "shift has to be between 1 and 32 inclusive"
             shift))
    (unless (register? reg)
      (error 'register/arithmetical-shift-right
             "not a valid register"
             reg))
    (imi:bitwise-copy-bit-field* 0
                                 0  4 reg
                                 4  7 #b100
                                 7 11 shift))


  #;
  (define register/arithmetical-shift-right-opcode
    (make-register-operand
      `(((11    7)   (6 5 4)   (3 0))
        (shift-imm   (1 0 0)     Rm )
        (    #f         #f       #f ))))



  ;;;  11        8   7  6  5  4   3        0
  ;;; +------------+------------+------------+
  ;;; |     Rs     | 0  1  0  1 |     Rm     |
  ;;; +------------+------------+------------+
  ;;;
  ;;; result: contents of `Rm` arithmetically
  ;;;         shifted right by amount given in
  ;;;         `Rs`
  (define (register/arithmetical-shift-right/register shift-reg reg)
    (unless (and (register? shift-reg)
                 (register? reg))
      (error 'register/arithmetical-shift-right/register
             "both arguments have to be registers"
             shift-reg reg))
    (imi:bitwise-copy-bit-field* 0
                                 0  4 reg
                                 4  8 #b0101
                                 8 12 shift-reg))

  #;
  (define register/arithmetical-shift-right/register-opcode
    (make-register-operand
      `(((11 8)   (7 6 5 4)   (3 0))
        (  Rs     (0 1 0 1)     Rm )
        (  #f         #f        #f ))))






  ;;;; Rotates
  

  ;;;  11           7   6  5  4   3        0
  ;;; +---------------+---------+------------+
  ;;; |   shift_imm   | 1  0  0 |     Rm     |
  ;;; +---------------+---------+------------+
  ;;;
  ;;; result: contents of `Rm` rotated right by
  ;;;         `shift_imm`
  (define (register/rotate-right shift reg)
    (unless (<= 1 shift 31)
      (error 'register/rotate-right
             "shift has to be between 1 and 31 inclusive"
             shift))
    (unless (register? reg)
      (error 'register/rotate-right
             "not a valid register"
             reg))
    (imi:bitwise-copy-bit-field* 0
                                 0  4 reg
                                 4  7 #b100
                                 7 12 shift))

  #;
  (define (shift-imm? shift)
    (<= 1 shift 31))

  #;
  (define register/rotate-right-opcode
    `(((11      7)   (6 5 4)   (3 0))
      ( shift-imm    (1 0 0)     Rm )
      (,shift-imm?    #f       #f )))


  ;;;  11        8   7  6  5  4   3        0
  ;;; +------------+------------+------------+
  ;;; |     Rs     | 0  1  1  1 |     Rm     |
  ;;; +------------+------------+------------+
  ;;;
  ;;; result: contents of `Rm` rotated right by
  ;;;         the amount given in `Rs`
  (define (register/rotate-right/register shift-reg reg)
    (unless (and (register? shift-reg)
                 (register? reg))
      (error 'register/rotate-right/register
             "both arguments have to be registers"
             shift-reg reg))
    (imi:bitwise-copy-bit-field* 0
                                 0  4 reg
                                 4  8 #b0111
                                 8 12 shift-reg))

  #;
  (define register/rotate-right/register-opcode
    (make-register-operand
      `(((11 8)   (7 6 5 4)   (3 0))
        (  Rs     (0 1 1 1)     Rm )
        (  #f         #f        #f ))))



  ;;;  11 10  9  8  7  6  5  4   3        0
  ;;; +------------------------+------------+
  ;;; | 0  0  0  0  0  1  1  0 |     Rm     |
  ;;; +------------------------+------------+
  ;;;
  ;;; result: contents of `Rm` shifted right
  ;;;         by one bit, replacing the highest
  ;;;         bit (bit 31) with the carry flag
  ;;;         and the bit shifted out as the 
  ;;;         carry out
  (define (register/rotate-right/extend reg)
    (unless (register? reg)
      (error 'register/rotate-right/extend
             "not a valid register"
             reg))
    (imi:bitwise-copy-bit-field* 0
                                 0  4 reg
                                 4 12 #b00000110))


  #;
  (define register/rotate-right/extend-opcode
    (make-register-operand
      `(((11 10 9 8 7 6 5 4)   (3 0))
        (( 0  0 0 0 0 1 1 0)     Rm )
        (        #f              #f ))))




  )

