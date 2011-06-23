#!r6rs

(library (imi asm arm opcodes memory)
  (export load store)
  (import (rnrs)
          (only (imi math bitwise bit-field)
                bitwise-copy-bit-field*)
          (imi asm arm opcodes register)
          (imi asm arm opcodes conditions)
          (imi asm arm opcodes addressing-mode2))

  ;;;; LOAD / STORE
  ;;;
  ;;; All instructions are conditional and
  ;;; work with an offset operand (addressing
  ;;; mode 2).
  ;;;
  ;;; Flag `L` determines if it is a load or store operation
  ;;; Flag `B` determines if it loads a word or an unsigned byte
  ;;;
  ;;;  31       28  27 26  25  24  23  22  21  20
  ;;; +------------+------+---+---+---+---+---+---+- ...
  ;;; |    cond    | 0  1 | I | P | U | B | W | L |  ...
  ;;; +------------+------+---+---+---+---+---+---+- ...
  ;;;  given by offset:     ^   ^   ^       ^
  ;;;
  ;;;       19       16  15       12  11                 0
  ;;; ... -+------------+------------+------  . . .  ------+
  ;;; ...  |     Rn     |     Rd     |  addr_mode_specific |
  ;;; ... -+------------+------------+------  . . .  ------+
  

  (define (load/store load? byte? dest-reg pos-reg offset)
    (unless (offset? offset)
      (error 'load/store
             "not a valid offset"
             offset))
    (unless (register? dest-reg)
      (error 'load/store
             "not a valid register"
             dest-reg))
    (unless (register? pos-reg)
      (error 'load/store
             "not a valid register"
             pos-reg))

    (bitwise-copy-bit-field* offset
                             12 16 dest-reg
                             16 20 pos-reg
                             20 21 (if load? 1 0)
                             22 23 (if byte? 1 0)
                             26 28 #b01))

  #;
  (define load/store-opcode
    `(((19 16)   (15 12)   (11             0))
      (  Rn        Rd      addr-mode-specific)
      (  #f        #f              #f        )))


  (define (load byte? dest-reg pos-reg offset)
    (AL (load/store #t byte? dest-reg pos-reg offset)))

  (define (store byte? dest-reg pos-reg offset)
    (AL (load/store #f byte? dest-reg pos-reg offset)))


  )

