(import (rnrs))

(define (meaning e r)
  (if (atom? e)
      (if (symbol? e) (meaning-reference e r)
                      (meaning-quote e r))
      (case (car e)
        [(quote)  (meaning-quote (cadr e) r)]
        [(if)     (meaning-if (cadr e) (caddr e) (cadddr e) r)]
        [(begin)  (meaning-sequence (cdr e) r)]
        [(set!)   (meaning-set! (cadr e) (caddr e) r)]
        [(lambda) (meaning-abstraction (cadr e) (cddr e) r)]
        [else     (meaning-application (car e) (cdr e) r)])))


(define (meaning-quote v r)
  (lambda (sr k)
    (k v)))

(define (meaning-if ec et ef r)
  (let ([mc (meaning ec r)]
        [mt (meaning et r)]
        [mf (meaning ef r)])
    (lambda (sr k)
      (mc sr (lambda (v)
               ((if v mt mf) sr k))))))


(define (meaning-sequence e* r)
  (if (pair? e*)
      (if (pair? (cdr e*))
          (meaning-sequence-some (car e*) (cdr e*) r)
          (meaning (car e*) r))
      (error 'meaning-sequence
             "empty sequence"
             e*)))

(define (meaning-sequence-some e e* r)
  (let ([m (meaning e r)]
        [m* (meaning-sequence e* r)])
    (lambda (sr k)
      (m sr (lambda (v) 
              (m* sr k))))))


(define (meaning-set! n e r)
  (let ([m (meaning e r)])
    (
