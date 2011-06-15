(library (imi list utils)
  (export length=?
          length>?
          length<?
          length<  ; deprecated
          sorted?

          length*

          split-on
          reverse-append

          take
          drop
          halves
          last)
  (import (rnrs)
          (imi sugar cut)
          (imi math))

  ;;; does the check `(= len (length ls))`
  ;;;  but much faster if `ls` is much longer
  ;;;  than `len`
  ;;;
  ;;; ls - list?
  ;;; len - positive-integer?
  ;;;  -> boolean?
  (define (length=? ls len)
    (cond
      [(zero? len) (null? ls)]
      [(null? ls) #f]
      [else (length=? (cdr ls) (sub1 len))]))

  ;;; checks if `(length ls)` is graeter than
  ;;;   `minlen` but much faster for large
  ;;;   `maxlen` and small `ls`
  ;;;
  ;;; ls - list?
  ;;; minlen - positive-integer?
  ;;;  -> boolean?
  (define (length>? ls minlen)
    (cond
      [(null? ls) #f]
      [(zero? minlen) #t]
      [else (length>? (cdr ls) (sub1 minlen))]))

  ;;; checks if `(length ls)` is smaller than
  ;;;   `maxlen` but much faster for small
  ;;;   `maxlen` and large `ls`
  ;;;
  ;;; ls - list?
  ;;; maxlen - positive-integer?
  ;;;  -> boolean?
  (define (length<? ls maxlen)
    (cond
      [(zero? maxlen) #f]
      [(null? ls) #t]
      [else (length<? (cdr ls) (sub1 maxlen))]))

  ;;; deprecated - not according to naming conventions;
  ;;;              use length<? instead
  ;;;  same functionality as length<? due to length<
  ;;;  being only renamed to length<?
  (define length< length<?)

  ;;; checks if `ls` is sorted in order `cmp`
  ;;;
  ;;; cmp - (-> any? lstype lstype)
  ;;; ls - (listof/c lstype)
  ;;;  -> boolean?
  (define (sorted? cmp ls)
    (or (length< ls 2)
        (and (cmp (car ls)
                  (cadr ls))
             (sorted? cmp (cdr ls)))))

  ;;; returns length of `ls` after it was flattened
  ;;;
  ;;; ls - list?
  ;;;  -> positive-integer?
  (define (length* ls)
    (let loop ([ls ls]
               [len 0])
      (cond
        [(null? ls) len]
        [(list? ls)
         (loop (cdr ls)
               (+ (loop (car ls)
                        0)
                  len))]
        [else
         (loop (cdr ls)
               (add1 len))])))

  ;;; returns two lists, first the part of `ls`
  ;;;  from beginning until the element where 
  ;;;  `proc` is true and the rest of the list
  ;;;  starting from that element
  ;;;
  ;;; proc - (-> any? elemtype)
  ;;; ls - (listof/c elemtype)
  ;;;  -> (values/c (listof/c elemtype)
  ;;;               (listof/c elemtype))
  (define (split-on proc ls)
    (let loop ([start '()]
               [rest ls])
      (cond
        [(null? rest)
         (values ls '())]
        [(proc (car rest))
         (values (reverse start)
                 rest)]
        [else
          (loop (cons (car rest)
                      start)
                (cdr rest))])))

  ;;; appends `rev` in reverse order to `ls`
  ;;;  the same as `(append (reverse rev) ls)`
  ;;;
  ;;; rev - list?
  ;;; ls - list?
  ;;;  -> list?
  (define (reverse-append rev ls)
    (let loop ([rev rev] [ls ls])
      (cond
        [(null? rev) ls]
        [else
         (loop (cdr rev)
               (cons (car rev)
                     ls))])))

  ;;; takes the first `n` elements of a list
  ;;;  and returns them as a new list
  ;;;
  ;;; n - positive-integer?
  ;;; ls - (listof/c elem)
  ;;;  -> (listof/c elem)
  (define (take n ls)
    (if (zero? n)
      '()
      (cons (car ls)
            (take (sub1 n)
                  (cdr ls)))))

  ;;; drops first `n` elements of a list
  ;;;  and returns the rest of the list;
  ;;;  same as list-tail
  ;;;
  ;;; n - positive-integer?
  ;;; ls - (listof/c elem)
  ;;;  -> (listof/c elem)
  (define (drop n ls)
    (if (zero? n)
      ls
      (drop (sub1 n)
            (cdr ls))))

  ;;; returns the two halves of a list
  ;;;
  ;;; ls -> (listof/c elem)
  ;;;  -> (values/c (listof/c elem)
  ;;;               (listof/c elem))
  (define (halves ls)
    (let loop ([left '()]
               [right ls]
               [double ls])
      (cond
        [(length<? double 2)
         (values (reverse left)
                 right)]
        [else
         (loop (cons (car right)
                     left)
               (cdr right)
               (cddr double))])))


  ;;; returns the last element of a list
  ;;;
  ;;; ls - (listof/c elem)
  ;;;  -> elem
  (define (last ls)
    (if (length=? ls 1)
      (car ls)
      (last (cdr ls))))


  )
