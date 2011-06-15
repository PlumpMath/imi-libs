(library (imi list dlist)
  (export make-elem
          elem-val
          elem-left
          elem-right
          elem-connect!
          elem-chain!
          elem-insert-after!
          elem-ref

          nil

          make-dls
          dls-start
          dls-end

          dls-first-elem
          dls-last-elem

          list->dlist
          dlist
          dlist-set!

          dlist-ref
          dlist-elem-ref
          dlist-slice
          
          dlist-print)
  (import (rnrs))

  ;;; DRAFT
  ;;;   usable but not important

  (define-record-type elem (fields val (mutable left) (mutable right))
                      (protocol
                        (lambda (new)
                          (lambda (val) 
                            (new val nil nil)))))

  (define (empty-elem) (make-elem nil))
  (define (empty-elem? e)
    (and (elem? e)
         (null? (elem-val e))))

  (define (elem-connect! left right)
    (elem-right-set! left right)
    (elem-left-set! right left))

  (define (elem-chain! left right . rest)
    (elem-connect! left right)
    (unless (null? rest)
      (apply elem-chain! right rest)))

  (define (elem-insert-after! left new)
    (let ([right (elem-right left)])
      (elem-chain! left new right)
      new))

  (define (elem-ref e n)
    (let loop ([e e] [pos 0])
      (cond
        [(< n pos)
         (loop (elem-left e) (- pos 1))]
        [(> n pos)
         (loop (elem-right e) (+ pos 1))]
        [(= n pos) e])))

  (define nil '())

  (define-record-type dls (fields start end))

  (define (list->dlist ls)
    (let ([start (empty-elem)]
          [end (empty-elem)]
          [elems (map make-elem ls)])
      (apply elem-chain! start (append elems (list end)))
      (make-dls start end)))

  (define (dlist . ls)
    (list->dlist ls))

  (define (dlist-set! dls0 dls1)
    (elem-connect! (dls-start dls0)
                   (dls-first-elem dls1))
    (elem-connect! (dls-last-elem dls1)
                   (dls-end dls0)))

  (define (dls-first-elem dls)
    (elem-right (dls-start dls)))

  (define (dls-last-elem dls)
    (elem-left (dls-end dls)))

  (define (dlist-ref dls n)
    (elem-val (dlist-elem-ref dls n)))

  (define (dlist-elem-ref dls n)
    (if (>= n 0)
      (elem-ref (dls-first-elem dls) n)
      (elem-ref (dls-end dls) n)))

  (define (dlist-slice dls left right)
    (make-dls (elem-left (dlist-elem-ref dls left))
              (elem-right (dlist-elem-ref dls right))))

  (define (dlist-print dls)
    (display "[")
    (let ([first-elem (elem-right (dls-start dls))]
          [end (dls-end dls)])
      (unless (eq? first-elem end)
        (display (elem-val first-elem))
        (do ([elem (elem-right first-elem) (elem-right elem)])
            ((eq? elem end))
          (display " ")
          (display (elem-val elem)))))
    (display "]"))

  )
