#!r6rs

(library (imi asm opcode)
  (export opcode-fill-in
          opcode->number)
  (import (rnrs)
          (match)
          (prefix (imi math bitwise bit-field)
                  imi:))

  (define opcode-line-pos car)
  (define opcode-line-name cadr)
  (define opcode-line-desc cddr)

  (define (opcode-fill-in opcode what replacement)
    (fold-right (lambda (opcode-line new-opcode)
                  (if (eq? what (opcode-line-name opcode-line))
                      (append (opcode-replace opcode-line replacement)
                              new-opcode)
                      (cons opcode-line new-opcode)))
                '()
                opcode))


  (define (opcode-replace opcode-line replacement)
    (if (list? replacement)
        replacement
        (list (list (opcode-line-pos opcode-line)
                    (process-replacement replacement
                                         (opcode-line-size opcode-line)
                                         (opcode-line-desc opcode-line))))))

  (define (opcode-line-size opcode-line)
    (match (opcode-line-pos opcode-line)
      [(,pos0 ,pos1)
       (guard (number? pos0)
              (number? pos1))
       (abs (- pos0 pos1))]
      [,bit
       (guard (number? bit))
       1]
      [,invalid-pos
       (error 'opcode-line-size
              "invalid opcode position"
              invalid-pos)]))

  (define (opcode-line-pos-start opcode-line)
    (match (opcode-line-pos opcode-line)
      [(,pos0 ,pos1)
       (guard (number? pos0)
              (number? pos1))
       (min pos0 pos1)]
      [,bit
       (guard (number? bit))
       bit]
      [,invalid-pos
       (error 'opcode-line-pos-start
              "invalid opcode position"
              invalid-pos)]))

  (define (opcode-line-pos-end opcode-line)
    (match (opcode-line-pos opcode-line)
      [(,pos0 ,pos1)
       (guard (number? pos0)
              (number? pos1))
       (max pos0 pos1)]
      [,bit
       (guard (number? bit))
       (+ bit 1)]
      [,invalid-pos
       (error 'opcode-line-pos-end
              "invalid opcode position"
              invalid-pos)]))


  (define (process-replacement replacement size desc)
    (fold-left (lambda (replacement desc)
                 (cond 
                   [(procedure? desc)
                    (desc replacement)]
                   [(eq? desc 'unsigned)
                    (if (<= 0
                            replacement
                            (- (expt 2 size) 1))
                        replacement
                        (error 'process-replacement
                               "invalid unsigned number size"
                               replacement size))]
                   [(eq? desc 'signed)
                    (if (< (- (expt 2 (- size 1)))
                           replacement
                           (- (expt 2 (- size 1)) 1))
                        replacement
                        (error 'process-replacement
                               "invalid signed number size"
                               replacement size))]
                   [(eq? desc 'bit)
                    (cond
                      [(not (= size 1))
                       (error 'process-replacement
                              "cannot be a bit - wrong size"
                              size)]
                      [(number? replacement)
                       (if (<= 0 replacement 1)
                           replacement
                           (error 'process-replacement
                                  "invalid bit as number"
                                  replacement))]
                      [else
                       (if replacement 1 0)])]
                   [else
                    (error 'process-replacement
                           "unknown description"
                           desc)]))
               replacement
               desc))
                   


  (define (opcode->number opcode)
    (fold-left (lambda (number opcode-line)
                 (cond
                   [(not (number? (opcode-line-name opcode-line)))
                    (error 'opcode->number
                           "opcode not filled in"
                           opcode-line)]
                   [else
                    (imi:bitwise-copy-bit-field
                      number
                      (opcode-line-pos-start opcode-line)
                      (opcode-line-pos-end opcode-line)
                      (opcode-line-name opcode-line))]))
               0
               opcode))


  )

