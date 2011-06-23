#!/usr/bin/env scheme-script

#!r6rs

(import (imi help)
        (imi asm utils)
        (imi string format)
        (imi utils print)
        (ikarus))

(define (truncate-depth what maxdep)
  (cond
    [(zero? maxdep) '...]
    [(pair? what)
     (if (= maxdep 1)
         '(...)
         (cons (truncate-depth 
                 (car what)
                 (- maxdep 1))
               (truncate-depth
                 (cdr what)
                 (- maxdep 1))))]
    [(vector? what)
     (list->vector
       (truncate-depth
         (vector->list what)
         maxdep))]
    [else what]))

(define (print/maxdepth what)
  (pretty-print (truncate-depth what 10)))

(define (print-found found)
  (newline)
  (for-each (lambda (found)
              (print (format "~s:"
                             (car found)))
              (print "  " (cdr found)))
            found)
  (print "----------------------"))

(pretty-width 40)

(let loop ()
  (newline)
  (display "search: ")
  (let ([in (read)])
    (unless (eof-object? in)
      (case-match in
        [(apropos (: id ,symbol?))
         (id)
         (apropos id)]
        [((or def definitions-of)
          (: lib ,list?))
         (lib)
         (for-each print/maxdepth
                   (definitions-of lib))]
        [(or def definitions-of)
         ()
         (case-match (read)
           [(: lib ,list?)
            (lib)
            (for-each print/maxdepth
                      (definitions-of lib))]
           [else
            ()
            (print "error: invalid def search")])]
        [(or (: id ,symbol?)
             (id (: id ,symbol?)))
         (id)
         (print-found
           (find-identifier id))]
        [else
         ()
         (print "error: invalid search")])
      (loop))))

(exit)
