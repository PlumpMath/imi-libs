#!r6rs

(library (imi list sort treesort)
  (export treesort
          make-leaf
          leaf-value
          leaf-left
          leaf-right
          leaf-depth-difference
          leaf->list)
  (import (rnrs)
          (rnrs mutable-pairs))

  (define-record-type leaf
                      (fields value
                              (mutable left)
                              (mutable right)
                              (mutable depth-difference)))

  (define nil '())
  (define nil? null?)

  (define (new-leaf value)
    (make-leaf value nil nil 0))

  (define (leaf-rotate-clockwise! head)
    (let* ([new-head (leaf-left head)]
           [head-left (leaf-right new-head)])
      (leaf-left-set! head head-left)
      (leaf-right-set! new-head head)
      new-head))

  (define (leaf-rotate-conterclockwise! head)
    (let* ([new-head (leaf-right head)]
           [head-right (leaf-left new-head)])
      (leaf-right-set! head head-right)
      (leaf-left-set! new-head head)
      new-head))

  (define (leaf-left-deeper? leaf)
    (< 0 (leaf-depth-difference leaf)))

  (define (leaf-right-deeper? leaf)
    (> 0 (leaf-depth-difference leaf)))

  (define (leaf-depth-balanced? leaf)
    (= 0 (leaf-depth-difference leaf)))

  (define (leaf-insert! less? head new-leaf)
    (cond
      [(less? (leaf-value new-leaf)
              (leaf-value head))
        (if (leaf-
