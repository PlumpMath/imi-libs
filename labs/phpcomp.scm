#!/usr/bin/env scheme-script

#!r6rs

(import (rnrs)
        (imi sugar list-let)
        (imi sugar receive)
        (imi proc fold-apply)
        (imi list processing))


(define (sum sth)
  (fold-apply + sth))

(define *maxlength* 60)

(define (strlength ls)
  (+ (sum (map string-length ls))
     (- (length ls) 1)))

;;; maxlen 3          0 2 4 
;;; ("a" "b" "c") => "a b c" len 5
(define (strformat pos ls)
  (if (null? (cdr ls))
      (values (+ pos (string-length (car ls)))
              ls)
      (let ([selflen (string-length (car ls))])
        (receive (len ls) (strformat (+ 1 selflen

