(library (imi asm core opcodes)
  (export make-opcode
          opcode?
          opcode-size
          opcode-extra
          opcode-code
          opcode-bytes
          
          bytevector->number
          number->bytevector
          bytevector-update!)
  (import (rnrs))

  ;;; to get the size of an asm opcode
  ;;;   (in bytes) and generate it
  ;;;
  ;;; size - positive-integer?
  ;;; generator - (->* integer? args)
  ;;; extra - list?
  (define-record-type opcode
    (fields
      size
      generator
      extra)
    (protocol
      (lambda (new)
        (lambda (size generator . extra)
          (unless (and (integer? size)
                       (> size 0))
            (error 'make-opcode "size has to be an positive integer" size))
          (unless (procedure? generator)
            (error 'make-opcode "generator has to be a procedure" generator))

          (new size generator extra)))))

  ;;; generates the opcode
  ;;;
  ;;; opcode - opcode?
  ;;; extra - args
  ;;;  -> integer?
  (define (opcode-code opcode . extra)
    (apply (opcode-generator opcode) 
           (opcode-extra opcode)
           extra))

  ;;; generates the opcode and converts it to a bytevector
  ;;;
  ;;; opcode - opcode?
  ;;; extra - args?
  ;;;  -> bytevector?
  (define (opcode-bytes opcode . extra)
    (let ([bytes (make-bytevector (opcode-size opcode))]
          [code (apply opcode-code opcode extra)])
      (bytevector-set! bytes 0 code
                       (endianness little)
                       (opcode-size opcode))
      bytes))

  ;;; converts a bytevector into a number (integer)
  ;;;
  ;;; bv - bytevector?
  ;;;  -> integer?
  (define (bytevector->number bv)
    (bytevector-ref bv 0
                    (endianness little)
                    (bytevector-length bv)))

  ;;; converts a number (integer) into a bytevector
  ;;;   of length `len`
  ;;;
  ;;; x - integer?
  ;;;  -> bytevector?
  (define (number->bytevector len x)
    (let ([bytes (make-bytevector len)])
      (bytevector-set! bytes 0 x
                       (endianness little)
                       len)
      bytes))

  ;;; changes a bytevector by calling its value as
  ;;;   number to `proc` assigning the return value
  ;;;   to itself
  ;;;
  ;;; bv - bytevector?
  ;;; proc - (-> integer? integer?)
  ;;;  -> bytevector?
  (define (bytevector-update! bv proc)
    (bytevector-set! bv 0
                     (proc (bytevector->number bv))
                     (endianness little)
                     (bytevector-length bv))
    bv)

  )
