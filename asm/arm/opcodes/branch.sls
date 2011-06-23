#!r6rs

(library (imi asm arm opcodes branch)
  (export branch
          branch/link)
  (import (rnrs)
          (only (imi math bitwise bit-field)
                bitwise-copy-bit-field*)
          (imi asm arm opcodes conditions))

  ;;;; BRANCH INSTRUCTIONS
  ;;;
  ;;; All standard branch instructions can be conditional
  ;;; of course. So they have the 4 bit cond header.  Some
  ;;; extended branch instructions take the reserved NV
  ;;; conditional to be encoded.
  ;;; The Instructions have the standard condition AL, which
  ;;; can be altered by passing the resulting instruction to
  ;;; another condition procedure in (imi asm arm conditions)
  ;;;
  ;;;  31       28
  ;;; +------------+- ...
  ;;; |    cond    |  ...
  ;;; +------------+- ...


  ;;;       27 26 25  24  23                            0
  ;;; ... -+---------+---+------------  . . .  -----------+
  ;;; ...  | 1  0  1 | L |         signed_immed_24        |
  ;;; ... -+---------+---+------------  . . .  -----------+
  ;;;
  ;;; result: a jump relative to the actual PC (which is the
  ;;;         branch instruction position + 8)
  ;;;
  ;;; NOTE: This procedure already produces the code, which
  ;;;       jumps to the position of this instruction + the
  ;;;       given offset, so you don't have to cope with
  ;;;       offset of the PC caused by pipelining
  ;;;
  ;;; The L flag/bit determines if the return address (Link)
  ;;; is stored in R14 (LR) on branch
  (define branch
    (case-lambda
      [(offset) (branch #f offset)]
      [(link? offset)
       (unless (valid-branch-offset24? offset)
         (error 'branch
                (string-append "invalid offset, has to be approximately "
                               "in a range of +-32MB; if it is larger you "
                               "have to use MOV sth R15/PC")
                offset))
       (AL (bitwise-copy-bit-field* 0
                                     0 24 (offset24 offset)
                                    24 25 (if link? 1 0)
                                    25 28 #b101))]))

  #;
  (define branch
    `(((27 26 25)   24   (23                      0))
      (( 1  0  1)    L   (signed-immed-24 ,offset24))
      (    #f       #f     ,valid-branch-offset24?  )))


  (define (valid-branch-offset24? offset)
    (and (zero? (bitwise-and #b11 offset))
         (< (- (expt 2 24))
            (offset24 offset)
            (expt 2 24))))

  (define (offset24 offset)
    (bitwise-arithmetic-shift-right
      (- offset 8)
      2))


  ;;; A procedure to create a branch with link instruction
  (define (branch/link offset)
    (branch #t offset))


  )

