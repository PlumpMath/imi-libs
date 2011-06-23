#!r6rs

(library (imi parser binary code)
  (export parse-binary-code
          collect-binary-codes)
  (import (rnrs)
          (imi math)
          (imi sugar cut))

  (define (mask-upper-bits code code-len mask-len)
    (bitwise-and
      (bitwise-arithmetic-shift-right
        code
        (- code-len mask-len))
      (sub1 (bitwise-arithmetic-shift-left 1 mask-len))))

  (define tree-mask-len car)
  (define tree-code cadr)
  (define tree-info cddr)

  (define (subtree-matcher code code-len)
    (lambda (subtree)
      (= (mask-upper-bits code code-len (tree-mask-len subtree))
         (tree-code subtree))))

  (define (parse-binary-code parse-info code)
    (let ([code-len (car parse-info)]
          [info (cdr parse-info)])
      (cond
        [(not (list? info)) info]
        [(find (subtree-matcher code code-len)
               info)
         => (lambda (match)
              (parse-binary-code
                (cons (- code-len (tree-mask-len match))
                      (tree-info match))
                code))]
        [else
         (error 'parse-binary-code
                "invalid code for parse-tree"
                info code)])))

  (define (prepend-section code-len tree subcode)
    (bitwise-ior
      (bitwise-arithmetic-shift-left
        (tree-code tree)
        (- code-len (tree-mask-len tree)))
      subcode))

  (define (prepend-section/collect code-len)
    (lambda (subtree)
      (map (lambda (subcode)
             (cons (car subcode)
                   (prepend-section code-len
                                    subtree
                                    (cdr subcode))))
           (collect-binary-codes
             (cons (- code-len (tree-mask-len subtree))
                   (tree-info subtree))))))

  (define (collect-binary-codes parse-info)
    (let ([code-len (car parse-info)]
          [tree-info (cdr parse-info)])
      (if (list? tree-info)
          (apply append
                 (map (prepend-section/collect code-len)
                      tree-info))
          (list (cons tree-info 0)))))

  )
