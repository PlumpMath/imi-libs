#!r6rs

(library (imi asm arm instructions branch)
  (export *branch-instructions*)
  (import (rnrs)
          (imi asm utils)
          (imi asm arm opcodes branch))


  ;;; parser for any branch instruction
  (define (any-parser instr)
    (lambda (label name args)
      (case-match args
        [((: offset))
         (offset)
         (instr (get-offset offset label name))]
        [else
         ()
         (error name "wrong argument count" args)])))





  ;;; the branch instructions
  (define *branch-instructions*
    `((B  . ,(any-parser branch))
      (BL . ,(any-parser branch/link))
      ))





  ;;; parses the offset of a branch
  (define (get-offset arg label name)
    (cond
      [(number? arg) arg]
      [(symbol? arg) (label arg)]
      [else (error name "wrong argument type" arg)]))

  )

