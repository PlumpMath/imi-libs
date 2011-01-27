(library (imi asm core labels)
  (export label
          label?
          label=?
          label-id

          label-searcher

          label-processor)
  (import (rnrs)
          (imi asm core opcodes))

  ;;; a label with a id
  ;;;
  ;;; id - symbol?
  (define-record-type (label-rtd label label?)
    (fields
      id)
    (protocol
      (lambda (new)
        (lambda (id)
          (unless (symbol? id)
            (error 'make-label "id has to be a symbol" id))
          (new id)))))

  ;;; compares if two labels are the same, means their
  ;;;   id is the same
  ;;;
  ;;; lbl0 - label?
  ;;; lbl1 - label?
  ;;;  -> boolean?
  (define (label=? lbl0 lbl1)
    (symbol=? (label-id lbl0)
              (label-id lbl1)))

  ;;; produces a procedure which checks if a label
  ;;;   is the label searched for
  ;;;
  ;;; lbl - label?
  ;;;  -> (-> boolean? label?)
  (define (label-searcher lbl)
    (lambda (lbl1)
      (label=? lbl lbl1)))

  ;;; processes labels from list of asm instructions
  ;;;   and symbols therein (= labels) and maps them
  ;;;   to their position (calculated by opcode-size)
  ;;;
  ;;; instructions - (listof/c (or/c label? opcode?))
  ;;;  -> (listof/c (cons/c label? integer?))
  (define (label-processor instructions)
    (let labels-at ([pos 0] [instr instructions])
      (cond
        [(null? instr) '()]
        [(label? (car instr))
         (cons
           (cons
             (car instr)
             pos)
           (labels-at pos (cdr instr)))]
        [(opcode? (car instr))
         (labels-at
           (+ pos (opcode-size (car instr)))
           (cdr instr))]
        [else
          (error 'label-processor "cannot process asm instruction" (car instr))])))

  )
