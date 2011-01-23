(library (imi asm core labels)
  (export label-processor)
  (import (rnrs)
          (imi asm core opcodes))

  ;;; processes labels from list of asm instructions
  ;;;   and symbols therein (= labels) and maps them
  ;;;   to their position (calculated by opcode-size)
  ;;;
  ;;; instructions - (listof/c (or/c symbol? opcode?))
  ;;;  -> (listof/c (cons/c symbol? integer?))
  (define (label-processor instructions)
    (let labels-at ([pos 0] [instr instructions])
      (cond
        [(null? instr) '()]
        [(symbol? (car instr))
         (cons
           (cons (car instr) pos)
           (labels-at pos (cdr instr)))]
        [(opcode? (car instr))
         (labels-at
           (+ pos (opcode-size (car instr)))
           (cdr instr))]
        [else
          (error 'label-processor "cannot process asm instruction" (car instr))])))

  )
