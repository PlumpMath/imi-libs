#!r6rs

(library (imi list processing largest-common-sequence)
  (export largest-common-sequence)
  (import (rnrs)
          (imi sugar receive))

  (define (largest-common-sequence eq? ls0 ls1)
    (cond
      [(or (null? ls0)
           (null? ls1))
       (values 0 '())]
      [(eq? (car ls0)
            (car ls1))
       (receive (len lcs-ls) (largest-common-sequence eq?
                                                      (cdr ls0)
                                                      (cdr ls1))
         (values (+ 1 len)
                 (cons (car ls0)
                       lcs-ls)))]
      [else
       (receive (len0 lcs-ls0) (largest-common-sequence eq?
                                                        ls0
                                                        (cdr ls1))
         (receive (len1 lcs-ls1) (largest-common-sequence eq?
                                                          (cdr ls0)
                                                          ls1)
           (if (> len0 len1)
               (values len0 lcs-ls0)
               (values len1 lcs-ls1))))]))

  )

