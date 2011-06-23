#!r6rs

(library (imi list)
  (export make-list
          for-all-sublist
          list-set!
          remp-first
          intersect
          length<)
  (import (rnrs)
          (rnrs mutable-pairs)
          
          (imi math)
          (imi sugar cut))

  ;;; DRAFT
  ;;;   - needs reorganization

  (define make-list
    (case-lambda
      [(len) (make-list len 0)]
      [(len elem)
       (cond
         [(zero? len) '()]
         [else 
           (cons elem
                 (make-list (sub1 len)
                            elem))])]))

  (define (for-all-sublist proc . ls)
    (let loop ([ls ls])
      (or (exists null? ls)
          (and (apply proc (map car ls))
               (loop (map cdr ls))))))

  (define (list-set! ls pos val)
    (set-car! (list-tail ls pos) val))

  (define (remp-first proc ls)
    (cond
      [(null? ls) ls]
      [(proc (car ls)) (cdr ls)]
      [else
        (cons (car ls)
              (remp-first proc (cdr ls)))]))

  (define (intersect eq ls0 ls1)
    (filter (lambda (elem)
              (memp (cut eq elem <>)
                    ls1))
            ls0))

  ;;; FIXME: already in (imi list utils)
  ;;;        fix dependencies and remove here
  (define (length< ls maxlen)
    (cond
      [(zero? maxlen) #f]
      [(null? ls) #t]
      [else (length< (cdr ls) (sub1 maxlen))]))

  )
