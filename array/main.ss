(library (imi array)
  (export make-array
          array?
          array-pos
          array-ref
          array-set!)
  (import (rnrs))

  (define (product ls)
    (fold-left * 1 ls))

  ;;; multidimensional array
  (define-record-type array
    (fields dimensions contents)
    (protocol
      (lambda (new)
        ;;; dimensions - (listof/c (number>/c 0))
        (lambda dimensions
          (new dimensions
               (make-vector (product dimensions)))))))


  ;;; calcs position of an element in a `(length dimensions)`
  ;;;   dimensional matrix with sizes `dimensions` at position
  ;;;   `specs`
  ;;;
  ;;; dimensions - (listof/c (number>/c 0))
  ;;; position - (listof/c (number>=/c 0))
  ;;;  -> (number>=/c 0)
  (define (array-pos dimensions specs)
    (let calc-pos ([dims dimensions] [specs specs]
                   [dim-size 1] [spec-pos 0])
      (cond
        [(null? dims) spec-pos]
        [else
          (calc-pos
            (cdr dims) (cdr specs)
            (* dim-size (car dims))
            (+ (* (car specs) dim-size)
               spec-pos))])))

  ;;; get an element out of `array` at position `specs`
  ;;;
  ;;; array - array?
  ;;; specs - (listof (number>=/c 0))
  ;;;  -> any?
  (define (array-ref array . specs)
    (vector-ref (array-contents array)
                (array-pos (array-dimensions array)
                           specs)))

  ;;; set an element in `array` at position `specs` to `val`
  ;;;
  ;;; array - array?
  ;;; val - any?
  ;;; specs - (listof (number>=/c 0))
  ;;;  -> void?
  (define (array-set! array val . specs)
    (vector-set! (array-contents array)
                 (array-pos (array-dimensions array)
                            specs)
                 val))

  )
