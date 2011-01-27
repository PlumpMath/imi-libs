(library (imi asm core register)
  (export make-register
          register?
          register-id
          register=?
          
          register-searcher)
  (import (rnrs))

  ;;; a wrapper to identify a register used
  ;;;
  ;;; id - symbol? ; identifying the register
  (define-record-type register
    (fields
      id)
    (protocol
      (lambda (new)
        (lambda (id)
          (unless (symbol? id)
            (error 'make-register "id has to be a symbol" id))
          (new id)))))

  ;;; compares two registers by their id
  ;;;
  ;;; reg0 - register?
  ;;; reg1 - register?
  ;;;  -> boolean?
  (define (register=? reg0 reg1)
    (symbol=? (register-id reg0)
              (register-id reg1)))

  ;;; produces a procedure which checks if
  ;;;   the given register is the searched
  ;;;
  ;;; reg - register?
  ;;;  -> (-> boolean? register?)
  (define (register-searcher reg)
    (lambda (reg0)
      (register=? reg reg0)))

  )
