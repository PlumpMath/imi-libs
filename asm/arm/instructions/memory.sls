#!r6rs

(library (imi asm arm instructions memory)
  (export *memory-instructions*)
  (import (rnrs)
          (imi sugar receive)
          (imi asm utils)
          (imi asm arm opcodes memory)
          (imi asm arm parser register)
          (imi asm arm parser offset-operand))


  ;;; parses any memory instr basically by checking
  ;;; for the right length so every element is there.
  ;;; It is independant of load or store instruction,
  ;;; as both have the same arguments
  (define (parse-memory instr)
    (lambda (label name args)
      (case-match args
        [((: dest-reg) (: operand))
         (dest-reg operand)
         (parse-any instr
                    label
                    #f
                    dest-reg
                    operand
                    'offset)]
        [((: dest-reg) (: operand) (: addr-mode))
         (dest-reg operand addr-mode)
         (parse-any instr
                    label
                    #f
                    dest-reg
                    operand
                    addr-mode)]
        [((: dest-reg) (: byte) (: operand) (: addr-mode))
         (dest-reg byte operand addr-mode)
         (parse-any instr
                    label
                    (parse-byte name byte)
                    dest-reg
                    operand
                    addr-mode)]
        [else
         ()
         (error name
                "invalid arguments"
                args)])))



  ;;; the memory instructions
  (define *memory-instructions*
    `((LDR . ,(parse-memory load))
      (STR . ,(parse-memory store))
      ))




  ;;; checks and parses if the operation
  ;;; should work on a/an (unsigned) byte
  ;;; or not (so on a word)
  (define (parse-byte who size)
    (case size
      [(W) #f]
      [(B) #t]
      [else (error who "invalid load size" size)]))



  ;;; parses either load or store, as they don't differ in
  ;;; their form but only the name and the result, so either
  ;;; load or store should be passed as `instr`
  (define (parse-any instr label byte? dest-reg operand addr-mode)
    (receive (sign pos-reg offset) (parse-operand operand)
      (cond
        [(get-register dest-reg)
         => (lambda (dest-reg)
              (instr byte?
                     dest-reg
                     pos-reg 
                     (parse-offset-operand label
                                           addr-mode
                                           sign
                                           offset)))]
        [else (error 'parse-any "invalid register" dest-reg)])))


  
  ;;; checks the offset operand if it is
  ;;; of the right form and partially parses
  ;;; its contents
  (define (parse-operand operand)
    (case-match operand
      [((: sign) (: reg) (: offset))
       (sign reg offset)
       (check sign sign? 'parse-operand "invalid sign")
       (check reg get-register 'parse-operand "invalid register")
       (values sign
               (get-register reg)
               offset)]
      [else
       ()
       (error 'parse-operand
              "wrong operand form"
              operand)]))


  ;;; checks if it is a sign
  (define (sign? sign)
    (case sign
      [(+ -) #t]
      [else #f]))



  )

