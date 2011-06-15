(library (imi list processing)
  (export list-chomp
          list-split
          list-split-at
          list-split-at*
          list-intersperse
          list-intertwine
          list-contains
          list-flatten
          list-merge-unique

          rempn
          
          map*
          deepmap
          intermap)
  (import (rnrs)
          (imi list utils)
          (imi math))

  ;;; removes sequence at the beginning of `ls`
  ;;;   where each element approves to `pred?`
  ;;;
  ;;; pred? - (-> any? pred)
  ;;; ls - (listof/c pred)
  ;;;  -> (listof/c pred)
  (define (list-chomp pred? ls)
    (if (pred? (car ls))
      (list-chomp pred? (cdr ls))
      ls))

  ;;; splits `ls` at the elements, which approve
  ;;;   to `pred?`
  ;;;
  ;;; pred? - (-> any? pred)
  ;;; ls - (listof/c pred)
  ;;;  -> (listof/c (listof/c pred))
  (define (list-split pred? ls)
    (let loop ([act '()] [ls ls] [splitted '()])
      (cond
        [(null? ls)
         (reverse (cons (reverse act)
                        splitted))]
        [(pred? (car ls))
         (loop '() (cdr ls) (cons (reverse act) splitted))]
        [else
          (loop (cons (car ls) act)
                (cdr ls)
                splitted)])))


  ;;; splits `ls` at the given positions, where
  ;;;  the given position is the first element
  ;;;  of the next part
  ;;;
  ;;; positions - (listof/c positive-integer?)
  ;;; ls - list?
  ;;;  -> (listof/c list?)
  (define (list-split-at positions ls)
    (let loop ([n 0]
               [part '()]
               [splits '()]
               [pos (list-sort < positions)]
               [rest ls])
      (cond
        [(null? pos)
         (reverse (cons rest
                        splits))]
        [(null? rest)
         (error 'list-split-at
                "positions not in list"
                positions)]
        [(= n (car pos))
         (loop n
               '()
               (cons (reverse part)
                     splits)
               (cdr pos)
               rest)]
        [else
         (loop (add1 n)
               (cons (car rest)
                     part)
               splits
               pos
               (cdr rest))])))


  ;;; like list-split-at but takes a variable
  ;;;  number of arguments instead of a list
  ;;;  of split positions; but sadly the
  ;;;  positions have to be given after the
  ;;;  list :/
  ;;;
  ;;; ls - list?
  ;;; positions - (listof/c positive-integer?)
  ;;;  -> (listof/c list?)
  (define (list-split-at* ls . positions)
    (list-split-at positions ls))


  ;;; intersperses every element of `ls` with
  ;;;   `token`
  ;;;
  ;;; token - tokenpred
  ;;; ls - (listof/c lspred)
  ;;;  -> (listof/c (or/c lspred tokenpred))
  (define (list-intersperse token ls)
    (if (length< ls 2)
      ls
      (cons* (car ls)
             token
             (list-intersperse token (cdr ls)))))

  ;;; intertwines two lists, similar to
  ;;;  interspersing a list, but every token
  ;;;  is the next element of the intertwine-list
  ;;;
  ;;; ls0 - (listof/c ls0type)
  ;;; ls1 - (listof/c ls1type)
  ;;;  -> (rec result (or/c null?
  ;;;                       (list*/c ls0type ls1type result)
  ;;;                       (list/c ls0type)))
  (define (list-intertwine ls0 ls1)
    (cond
      [(null? ls1)
       (if (length>? ls0 1)
         (error 'list-intertwine
                "ls0 may only be one element larger than ls1"
                ls0 ls1)
         ls0)]
      [(null? ls0)
       (if (null? ls1)
         '()
         (error 'list-intertwine
                "ls1 may only be as large as ls0 or one elemnt shorter"
                ls0 ls1))]
      [else
       (cons* (car ls0)
              (car ls1)
              (list-intertwine (cdr ls0)
                               (cdr ls1)))]))


  ;;; checks if `ls0` contains `ls1`
  ;;;
  ;;; eq? - (-> any? pred pred)
  ;;; ls0 - (listof/c pred)
  ;;; ls1 - (listof/c pred)
  ;;;  -> boolean?
  (define (list-contains eq? ls0 ls1)
    (define (list-cmp ls0 ls1)
      (cond
        [(null? ls1) #t]
        [(null? ls0) #f]
        [(eq? (car ls0) (car ls1))
         (list-cmp (cdr ls0) (cdr ls1))]
        [else #f]))

    (cond
      [(null? ls0) #f]
      [(list-cmp (cdr ls0) (cdr ls1))]
      [else
        (list-contains eq? (cdr ls0) ls1)]))


  ;;; flattens `ls` so that it has no sublists
  ;;;
  ;;; ls - list?
  ;;;  -> (listof/c (neg list?))
  (define (list-flatten ls)
    (cond
      [(null? ls) '()]
      [(list? (car ls))
       (append (list-flatten (car ls))
               (list-flatten (cdr ls)))]
      [else
       (cons (car ls)
             (list-flatten (cdr ls)))]))


  ;;; removes elements which aren't unique
  ;;;  from a list, where all same elements
  ;;;  are grouped (e.g. sorted)
  ;;;
  ;;; eq? - (-> any? elemtype elemtype)
  ;;; ls - (listof/c elemtype)
  ;;;  -> (listof/c elemtype)
  (define (list-merge-unique eq? ls)
    (if (null? ls)
      '()
      (let loop ([elem (car ls)]
                 [ls (cdr ls)])
        (cond
          [(null? ls) (list elem)]
          [(eq? elem (car ls))
           (loop elem (cdr ls))]
          [else
            (cons elem
                  (loop (car ls) (cdr ls)))]))))


  ;;; removes the `n` first occurences of 
  ;;;  elements in a list which approve to
  ;;;  `rem?`
  ;;;
  ;;; n - positive-integer?
  ;;; rem? - (-> any? elemtype)
  ;;; ls - (listof/c elemtype)
  ;;;  -> (listof/c elemtype)
  (define (rempn n rem? ls)
    (cond
      [(null? ls) '()]
      [(zero? n) ls]
      [(rem? (car ls))
       (rempn (sub1 n)
              rem?
              (cdr ls))]
      [else
       (cons (car ls)
             (rempn n
                    rem?
                    (cdr ls)))]))


  ;;; works the same as map, but calls `proc`
  ;;;  with the position and element
  ;;;
  ;;; proc - (-> result positive-integer? elem)
  ;;; ls - (listof/c elem)
  ;;;  -> (listof/c result)
  (define (map* proc ls)
    (let loop ([n 0] [res '()] [ls ls])
      (cond
        [(null? ls) (reverse res)]
        [else (loop (add1 n)
                    (cons (proc n (car ls))
                          res)
                    (cdr ls))])))


  ;;; similar to map, but works recursive:
  ;;;  if an element is a list that sublist
  ;;;  will be mapped the same way again
  ;;;
  ;;; proc - (-> result input)
  ;;; ls - (rec lsin (listof/c (or/c input lsin)))
  ;;;  -> (rec lsout (listof/c (or/c result lsout)))
  (define (deepmap proc ls)
    (cond
      [(null? ls) '()]
      [(list? (car ls))
       (cons (deepmap proc (car ls))
             (deepmap proc (cdr ls)))]
      [else
       (cons (proc (car ls))
             (deepmap proc (cdr ls)))]))


  ;;; similar to map, but calls the procedure
  ;;;  with a set of elements, whose number is
  ;;;  specified by `n`, which are standing next
  ;;;  to each other
  ;;;
  ;;; n - positive-integer?
  ;;; proc - (-> result input input)
  ;;; ls - (listof/c input)
  ;;;  -> (listof/c result)
  (define (intermap n proc ls)
    (cond
      [(length<? ls n) '()]
      [else
       (cons (apply proc (take n ls))
             (intermap n proc (cdr ls)))]))


  )
