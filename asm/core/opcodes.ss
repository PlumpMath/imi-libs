(library (imi asm core opcodes)
  (export make-opcode
          opcode?
          opcode-size
          opcode-bytes)
  (import (rnrs))

  ;;; to get the size of an asm opcode and generate it
  ;;;
  ;;; size - positive-integer?
  ;;; generator - (->* integer? args)
  (define-record-type opcode
    (fields
      size
      generator)
    (protocol
      (lambda (new)
        (lambda (size generator)
          (unless (and (integer? size)
                       (> size 0))
            (error 'make-opcode "size has to be an positive integer" size))
          (unless (procedure? generator)
            (error 'make-opcode "generator has to be a procedure" generator))

          (new size generator)))))

  ;;; calls the opcode generator
  ;;;
  ;;; opcode - opcode?
  ;;; extra - args
  ;;;  -> integer?
  (define (opcode-bytes opcode . extra)
    (apply (opcode-generator opcode) extra))

  )
