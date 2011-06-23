#!r6rs

(library (imi asm arm parser conditions)
  (export condition-instruction?
          set-condition
          *conditions*)
  (import (rnrs)
          (imi asm arm opcodes conditions))

  (define *cond-names*
    '(EQ ZS  NE ZC
      HS CS  LO CC
      MI NS  PL NC
         VS     VC

      HI     LS
      GE     LT
      GT     LE
      AL    ;NV
      ))

  (define *cond-procs*
    (list EQ ZS  NE ZC
          HS CS  LO CC
          MI NS  PL NC
             VS     VC

          HI     LS
          GE     LT
          GT     LE
          AL    ;NV
          ))

  (define *conditions*
    (map cons *cond-names* *cond-procs*))


  (define (condition-instruction? sth)
    (and (memq sth *cond-names*)
         #t))


  (define (set-condition condition opcode)
    (cond
      [(assq condition *conditions*)
       => (lambda (con)
            ((cdr con)
             opcode))]
      [else
       (error 'set-condition
              "invalid condition name"
              condition)]))

  )

