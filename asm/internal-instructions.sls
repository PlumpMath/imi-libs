#!r6rs

(library (imi asm internal-instructions)
  (export internal-instruction?
          parse-internal-instruction
          internal-instruction-size)
  (import (rnrs)
          (imi sugar cut)
          (imi asm utils))


  (define (internal-instruction? sth)
    (exists (cut <> sth)
            (list align?
                  )))


  (define (parse-internal-instruction instr pos)
    (cond
      [(align? instr)
       (parse-align instr pos)]
      [else
       (error 'parse-internal-instruction
              "invalid internal instruction"
              instr)]))

  (define (internal-instruction-size instr pos)
    (cond
      [(align? instr)
       (align-length instr pos)]
      [else
       (error 'internal-instruction-size
              "invalid internal instruction"
              instr)]))




  (define (align? sth)
    (case-match sth
      [(align ,number?) () #t]
      [else () #f]))


  (define (align-length sth pos)
    (case-match sth
      [(align (: align-on ,number?))
       (align-on)
       (- align-on
          (mod pos align-on))]
      [else
       ()
       (error 'align-length
              "invalid align instruction"
              sth)]))


  (define (parse-align instr pos)
    (make-bytevector
      (align-length instr pos)))

  )

