(library (imi asm core opcodes)
  (export make-opcode
          opcode?
          opcode-size
          opcode-bytes)
  (import (rnrs))

  ;;; to get the size of an asm opcode
  ;;;   (in bytes) and generate it
  ;;;
  ;;; size - positive-integer?
  ;;; generator - (->* bytevector? args)
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
  ;;;  -> bytevector?
  (define (opcode-bytes opcode . extra)
    (let ([bytes (make-bytevector (opcode-size opcode))]
          [code (apply (opcode-generator opcode) extra)])
      (bytevector-set! bytes 0 code
                       (endianness little)
                       (opcode-size opcode))
      bytes))

  )
