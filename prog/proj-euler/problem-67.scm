#!r6rs

#!/usr/bin/env scheme-script

#!r6rs

(import (rnrs))

(define (read-numbers)
  (let ([num (read)])
    (if (eof-object? num)
        '()
        (cons num
              (read-numbers)))))

(define (numbers->triangle ls)
  (let loop ([n 1]
             [ls ls])
    (if (null? ls)
        '()
        (let-values ([(row rest)
                      (take/drop n ls)])
          (cons row
                (loop (+ 1 n)
                      rest))))))

(define (max-path rtri)
  (if (null? (cdr rtri))
      (caar rtri)
      (max-path
        (cons (map + (cadr rtri)
                     (map max*
                          (row->pairs
                            (car rtri))))
              (cddr rtri)))))

(define (row->pairs row)
  (if (null? (cdr row))
      '()
      (cons (list (car row)
                  (cadr row))
            (row->pairs (cdr row)))))

(define (max* ls)
  (apply max ls))

(define (take/drop n ls)
  (let loop ([n n]
             [taken '()]
             [ls ls])
    (if (zero? n)
        (values (reverse taken)
                ls)
        (loop (- n 1)
              (cons (car ls)
                    taken)
              (cdr ls)))))

(define (comp1 p1 p2)
  (lambda (x) (p1 (p2 x))))

(write
  (max-path
    (reverse
      (numbers->triangle
        (with-input-from-file
          (cadr (command-line))
          read-numbers)))))
(newline)
