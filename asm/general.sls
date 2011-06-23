#!r6rs

(library (imi asm general)
  (export make-assembler)
  (import (rnrs)
          (imi sugar cut)
          (imi sugar receive)
          (imi list alist)
          (imi list processing)
          (imi asm internal-instructions))


  ;;; creates a simple assembler parsing procedure, which
  ;;; preprocesses its input with `preprocessor`, handles
  ;;; labels on its own, has some built-in special
  ;;; instructions (by now only align) and parses the
  ;;; unknown instructions with `dispatcher`
  (define (make-assembler opcode-size preprocessor parser)
    (lambda (code)
      (receive (labels code) (extract-labels opcode-size
                                             (if preprocessor
                                                 (preprocessor code)
                                                 code))
        (let ([label-resolver (alist->getter labels)])
          (fold-left bytevector-append
                     (make-bytevector 0)
                     (map* (internal-parser opcode-size
                                            label-resolver
                                            parser)
                           code))))))



  ;;; the internal parser which either tries to parse
  ;;; its own internal instruction or lets `parser` do
  ;;; the job
  (define (internal-parser opcode-size label-resolver parser)
    (lambda (pos code)
      (cond
        [(internal-instruction? code)
         (parse-internal-instruction code pos)]
        [else
         (parser (lambda (label)
                   (- (label-resolver label)
                      (* opcode-size pos)))
                 code)])))




  ;;; extracts the labels from `ls`, while
  ;;; counting every item in `ls` `opcode-size`
  ;;; up (so every item in `ls` has the size
  ;;; `opcode-size`)
  (define (extract-labels opcode-size ls)
    (let loop ([pos 0]
               [labels '()]
               [code '()]
               [ls ls])
      (cond
        [(null? ls)
         (values labels
                 (reverse code))]
        [(symbol? (car ls))
         (loop pos
               (cons (cons (car ls)
                           pos)
                     labels)
               code
               (cdr ls))]
        [(internal-instruction? (car ls))
         (loop (+ (internal-instruction-size
                    (car ls)
                    pos)
                  pos)
               labels
               code
               (cdr ls))]
        [else
         (loop (+ opcode-size pos)
               labels
               (cons (car ls)
                     code)
               (cdr ls))])))





  ;;; appends two bytevectors
  (define (bytevector-append bv0 bv1)
    (u8-list->bytevector
      (append (bytevector->u8-list bv0)
              (bytevector->u8-list bv1))))

  )

