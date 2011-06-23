#!r6rs

(library (imi prog dev repl)
  (export get-defines
          print-defines
          def-unquote
          def-std)
  (import (except (rnrs) unquote)
          (imi iter construct)
          (imi iter iterators)
          (imi utils print)
          (imi sugar cut))

  ;;;;;;;;;;;;;;;;;;;  D R A F T S  ;;;;;;;;;;;;;;;;;;


  ;;;;;;; HELPER UTILS ;;;;;;;;

  (define (get-defines file)
    (define (define-identifier def)
        (cadr def))

    (define (define? ls)
      (symbol=? (car ls) 'define))

    (with-input-from-file
      file
      (lambda ()
        (map define-identifier
             (filter define?
                     (iter-construct-list
                       (iter-input)))))))

  (define (print-defines file)
    (for-each (cut print "(define" <> "...")
              (get-defines file)))

  (define-syntax def-unquote
    (lambda (stx)
      (syntax-case stx ()
        [(_ [name action] ...)
         #'(define-syntax unquote ;doesn't work, doesn't work, doesn't work!!
             (lambda (stx)
               (syntax-case stx (name ...)
                 [(_ name) #'action]
                 ...)))])))

  (define-syntax def-std
    (lambda (stx)
      (syntax-case stx ()
        [(_ file)
         #'(def-unquote
             [r (load file)]
             [l (print-defines file)])])))

  )
