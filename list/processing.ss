#!r6rs

(library (imi list processing)
  (export list-chomp
          list-split
          list-split-between
          list-split-at
          list-split-at*
          list-intersperse
          list-intertwine
          list-contains
          list-flatten
          list-merge-unique

          list-prepend ;DRAFT
          list-postpend ;DRAFT

          list-cmp ;DRAFT
          list-match-start ;DRAFT
          list-match-partial ;DRAFT

          rempn
          
          map*
          map/rest ;DRAFT
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

  ;;; splits `ls` between the nearby elements which
  ;;;   approve to `split?`
  ;;;
  ;;; split? - (-> any? elem elem)
  ;;; ls - (listof/c elem)
  ;;;  -> (listof/c (listof/c elem))
  (define (list-split-between split? ls)
    (let loop ([act '()] [ls ls] [splitted '()])
      (cond
        [(null? ls)
         (reverse (cons (reverse act)
                        splitted))]
        [(null? act)
         (loop (cons (car ls)
                     act)
               (cdr ls)
               splitted)]
        [(split? (car act)
                 (car ls))
         (loop '()
               ls
               (cons (reverse act)
                     splitted))]
        [else
         (loop (cons (car ls)
                     act)
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
  ;;; outerls - (listof/c outertype)
  ;;; innerls - (listof/c innertype)
  ;;;  -> (rec result (or/c null?
  ;;;                       (list*/c outertype innertype result)
  ;;;                       (list/c outertype)))
  (define (list-intertwine outerls innerls)
    (cond
      [(null? innerls)
       (if (length>? outerls 1)
         (error 'list-intertwine
                "outerls may only be one element larger than innerls"
                outerls innerls)
         outerls)]
      [(null? outerls)
       (if (null? innerls)
         '()
         (error 'list-intertwine
                "innerls may only be as large as outerls or one element shorter"
                outerls innerls))]
      [else
       (cons* (car outerls)
              (car innerls)
              (list-intertwine (cdr outerls)
                               (cdr innerls)))]))


  ;;; checks if `largels` contains `subls`
  ;;;
  ;;; eq? - (-> any? pred pred)
  ;;; largels - (listof/c pred)
  ;;; subls - (listof/c pred)
  ;;;  -> boolean?
  (define (list-contains eq? largels subls)
    (define (list-cmp largels subls)
      (cond
        [(null? subls) #t]
        [(null? largels) #f]
        [(eq? (car largels) (car subls))
         (list-cmp (cdr largels) (cdr subls))]
        [else #f]))

    (cond
      [(null? largels) #f]
      [(list-cmp (cdr largels) (cdr subls))]
      [else
        (list-contains eq? (cdr largels) subls)]))


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



  ;;; - - - - - - - D R A F T - - - - - - - -
  ;;; inserts the element `elem` before each
  ;;;   element in `ls`
  ;;;
  ;;; elem - elemtype
  ;;; ls - (listof/c lstype)
  ;;;  -> (listof/c (or/c elemtype lstype))
  (define (list-prepend elem ls)
    (if (null? ls)
        '()
        (cons* elem
               (car ls)
               (list-prepend elem (cdr ls)))))


  ;;; - - - - - - - D R A F T - - - - - - - -
  ;;; inserts the element `elem` after each
  ;;;   element in `ls`
  ;;;
  ;;; elem - elemtype
  ;;; ls - (listof/c lstype)
  ;;;  -> (listof/c (or/c elemtype lstype))
  (define (list-postpend elem ls)
    (if (null? ls)
        '()
        (cons* (car ls)
               elem
               (list-postpend elem (cdr ls)))))



  ;;; - - - - - - D R A F T - - - - - - -
  ;;; compares two lists element by element
  ;;;  like string compares work character
  ;;;  by character
  ;;;
  ;;;  cmp - (-> any? elemtype0 elemtype1)
  ;;;  ls0 - (listof/c elemtype0)
  ;;;  ls1 - (listof/c elemtype1)
  ;;;   -> boolean?
  (define (list-cmp cmp ls0 ls1)
    (cond
      [(or (null? ls0)
           (null? ls1))
       (if (and (null? ls0)
                (null? ls1))
           #f
           (error 'list-cmp
                  "list lengths differ"
                  ls0 ls1))]
      [(cmp (car ls0)
            (car ls1))]
      [(cmp (car ls1)
            (car ls0))
       #f]
      [else 
       (list-cmp cmp
                 (cdr ls0)
                 (cdr ls1))]))


  ;;; - - - - - - - - D R A F T - - - - - - - - -
  ;;; checks if `ls` starts with the elements
  ;;;  as in `start`, so that `ls` is something
  ;;;  like `(append start rest)` (in simple cases,
  ;;;  if `eq?` is not just a simple checking
  ;;;  procedure, the result can of course vary)
  ;;;  returns either the rest of `ls` which is
  ;;;  after the starting sequence like in `start`
  ;;;  or false if the lists don't match
  ;;;
  ;;; eq? - (-> any? starttype lstype)
  ;;; start - (listof/c starttype)
  ;;; ls - (listof/c lstype)
  ;;;  -> (or/c (listof/c lstype) #f)
  (define (list-match-start eq? start ls)
    (cond
      [(null? start) ls]
      [(null? ls) #f]
      [(eq? (car start)
            (car ls))
       (list-match-start eq? (cdr start) (cdr ls))]
      [else #f]))


  ;;; checks if the elements of `part` are somewhere
  ;;;  in `ls` in the given order, so it checks if
  ;;;  `part` is a sublist of `ls`
  ;;;
  ;;; eq? - (-> any? parttype lstype)
  ;;; part - (listof/c parttype)
  ;;; ls - (listof/c lstype)
  ;;;  -> boolean?
  (define (list-match-partial eq? part ls)
    (cond
      [(null? ls) #f]
      [(list-match-start eq? part ls) #t]
      [else 
       (list-match-partial eq? part (cdr ls))]))


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


  ;;; - - - - - - - DRAFT - - - - - - - -
  ;;; maps the list like map, but gives
  ;;;  as a second argument to proc the
  ;;;  rest of the list
  ;;;
  ;;;  proc - (-> result elem (listof/c elem))
  ;;;  ls - (listof/c elem)
  ;;;   -> (listof/c result)
  (define (map/rest proc ls)
    (if (null? ls)
        '()
        (cons (proc (car ls)
                    (cdr ls))
              (map/rest proc (cdr ls)))))


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
