#!r6rs

(library (imi tree binary)
  (export make-node

          node-compare
          node-value

          node-left
          node-left-set!

          node-right
          node-right-set!)
  (import (rnrs)
          (imi sugar rec))

  (define (tree? obj)
    (or (node? obj)
        (null? obj)))

  (define-record-type node
    (fields
      comp-proc
      value
      (mutable left)
      (mutable right))
    (protocol
      (lambda (new)
        (rec mk
          (case-lambda
            [(comp-proc value)
             (mk comp-proc value '() '())]
            [(comp-proc value left right)
             (unless (and (tree? left)
                          (tree? right))
               (error 'make-node "left and right leaves have to be trees" left right))
             (new compare
                  value
                  left right)])))))

  ;;; TODO: rotate nodes, insert nodes, search nodes

  (define (subnodes node)
    (values (node-left node)
            (node-right node)))

  (define (node-rotate-right! top)
    (let* ([top-left (node-left top)]
           [top-left-right (node-right top-left)])
      (node-right-set! top-left top)
      (node-left-set! top top-left-right)
      top-left))

  (define (node-rotate-left! top)
    (let* ([top-right (node-right top)]
           [top-right-left (node-left top-right)])
      (node-left-set! top-right top)
      (node-right-set! top top-right-left)
      top-right))

  (define (node-height node)
    (if (null? node)
      0
      (add1
        (max
          (node-height
            (node-left
              node))
          (node-height
            (node-right
              node))))))

  (define (node-compare node val)
    ((node-comp-proc node)
     val
     (node-value node)))

  (define (node-make-left! node val)
    (node-left-set!
      node
      (make-node
        (node-comp-proc node)
        val)))

  (define (node-make-right! node val)
    (node-right-set!
      node
      (make-node
        (node-comp-proc node)
        val)))

  (define (node-insert! node val)
    (case (node-compare node val)
      [(<) (if (null? (node-left node))
             (node-make-left! node val)
             ; TODO

  )
