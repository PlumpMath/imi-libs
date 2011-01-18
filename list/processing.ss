(library (imi list processing)
  (export list-chomp
          list-split
          list-intersperse
          list-contains)
  (import (rnrs)
          (imi list utils))

  ;;; removes sequence at the beginning of `ls`
  ;;;   where each element approves to `pred?`
  ;;;
  ;;; pred? - (-> any? pred)
  ;;; ls - (listof/c pred)
  ;;;  -> (listof/c pred)
  (define (list-chomp pred? ls)
    (if (pred? (car ls))
      (list-chomp proc (cdr ls))
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

  )
