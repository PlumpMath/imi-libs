#!r6rs

(library (imi asm arm opcodes addressing-mode2)
  (export offset?
          immediate-offset
          register-offset
          scaled-register-offset)
  (import (rnrs)
          (imi sugar receive)
          (imi string format)
          (only (imi math bitwise bit-field)
                bitwise-mask**
                bitwise-copy-bit-field*)
          (imi asm arm opcodes register))

  ;;;; ADDRESSING MODE 2
  ;;;
  ;;; This addressing mode is used for Load and Store
  ;;; instructions. It has, just like addressing mode
  ;;; 1 the `I` bit, four flag bits `I`, `P`, `U` and
  ;;; `W`, responsible for the behaviour of addressing.
  ;;;
  ;;;  31       28  27 26  25  24  23  22  21  20
  ;;; +------------+------+---+---+---+---+---+---+- ...
  ;;; |    cond    | 0  1 | I | P | U | B | W | L |  ...
  ;;; +------------+------+---+---+---+---+---+---+- ...
  


  (define (offset? sth)
    (zero? (bitwise-and
             sth
             (bitwise-not
               (bitwise-mask**  0 12
                               21 22
                               23 26)))))




  ;;; NOTE: In the following, irrelevant flags will be marked by a '?'



  ;;;       25  24  23  22  21  20   19     12  11                    0
  ;;; ... -+---+---+---+---+---+---+--- ... ---+-------  . . .  --------+
  ;;; ...  | 0 | P | U | ? | W | ? |    ...    |        offset_12       |
  ;;; ... -+---+---+---+---+---+---+--- ... ---+-------  . . .  --------+
  ;;;
  ;;; result: an immediate offset given by `offset_12`
  (define (immediate-offset addr-mode offset)
    (unless (< (- (expt 2 12))
               offset
               (expt 2 12))
      (error 'immediate-offset
             "offset is to large, has to be between +-2^12 exclusive"
             offset))
    (receive (P W) (addressing-mode addr-mode)
      (bitwise-copy-bit-field* 0
                                0 12 (abs offset)
                               21 22 W
                               23 24 (if (negative? offset) 0 1)
                               24 25 P
                               25 26 0)))




  ;;;   25  24  23  22  21  20          11 10  9  8  7  6  5  4   3        0
  ;;; -+---+---+---+---+---+---+- ... -+------------------------+------------+
  ;;;  | 1 | P | U | ? | W | ? |  ...  | 0  0  0  0  0  0  0  0 |     Rm     |
  ;;; -+---+---+---+---+---+---+- ... -+------------------------+------------+
  ;;;
  ;;; result: an offset given by the register `Rm`
  (define (register-offset add? addr-mode reg)
    (unless (register? reg)
      (error 'register-offset
             "not a valid register"
             reg))
    (receive (P W) (addressing-mode addr-mode)
      (bitwise-copy-bit-field* 0
                                0  4 reg
                               21 22 W
                               23 24 (if add? 1 0)
                               24 25 P
                               25 26 1)))



  ;;;   25  24  23  22  21  20          11       7   6  5   4   3        0
  ;;; -+---+---+---+---+---+---+- ... -+--- . . ---+------+---+------------+
  ;;;  | 1 | P | U | ? | W | ? |  ...  | shift_imm | shift| 0 |     Rm     |
  ;;; -+---+---+---+---+---+---+- ... -+--- . . ---+------+---+------------+
  ;;;
  ;;; result: an offset given by the shifted register `Rm`
  (define (scaled-register-offset add? addr-mode shift-mode shift reg)
    (unless (register? reg)
      (error 'scaled-register-offset
             "not a valid register"
             reg))
    (let ([shift (parse-register-shift shift-mode shift)])
      (receive (P W) (addressing-mode addr-mode)
        (bitwise-copy-bit-field* 0
                                  0  4 reg
                                  4  5 0
                                  5 12 shift
                                 21 22 W
                                 23 24 (if add? 1 0)
                                 24 25 P
                                 25 26 1))))


  (define (parse-register-shift shift-mode shift)
    (case shift-mode
      [(LSL) (create-lsl-shift shift)]
      [(LSR) (create-lsr-shift shift)]
      [(ASR) (create-asr-shift shift)]
      [(ROR) (create-ror-shift shift)]
      [(RRX) (create-rrx-shift shift)]
      [else (error 'parse-register-shift
                   "invalid shift mode"
                   shift-mode)]))

  (define (shift-creator name minlen maxlen code shiftproc)
    (lambda (shift)
      (unless (<= minlen shift maxlen)
        (error name
               (format "shift must be between ~s and ~s inclusive"
                       minlen
                       maxlen)
               shift))
      (bitwise-copy-bit-field code 2 7 (shiftproc shift))))

  (define (ident x) x)
  (define (remap32 x)
    (if (= 32 x)
        0
        x))

  (define create-lsl-shift (shift-creator 'lsl-shift 0 31 #b00 ident)) 
  (define create-lsr-shift (shift-creator 'lsr-shift 1 31 #b01 remap32)) 
  (define create-asr-shift (shift-creator 'asr-shift 1 32 #b10 remap32)) 
  (define create-ror-shift (shift-creator 'ror-shift 1 31 #b11 ident))

  (define (create-rrx-shift shift)
    (unless (= shift 1)
      (error 'create-rrx-shift
             "rrx shift can only shift by one position a time"
             shift))
    (bitwise-copy-bit-field* #b11 2 7 0))




  (define (addressing-mode mode)
    (case mode
      [(post-indexed post-indexed/normal)
       (values 0 0)]
      [(post-indexed/privileged)
       (values 0 1)]
      [(offset)
       (values 1 0)]
      [(pre-indexed)
       (values 1 1)]
      [else
       (error 'addressing-mode
              "invalid addressing mode"
              mode)]))


  )

