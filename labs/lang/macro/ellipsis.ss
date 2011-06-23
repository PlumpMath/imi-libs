(import (imi labs lang datatype))

(define (any? x) #t)

(define-datatype pattern?
  (pattern-variable (name symbol?))
  (pattern-ellipsis (subpattern pattern?) (restpattern pattern?))
  (pattern-pair (a pattern?) (d pattern?))
  (pattern-constant (value any?))
  )


(define (parse-pattern input keywords)
  (cond
    [(symbol? input)
     (if (memq input keywords)
         (pattern-constant input)
         (pattern-variable input))]
    [(pair? input)
     (if (and (pair? (cdr input))
              (eq? '... (cadr input)))
         (pattern-ellipsis
           (parse-pattern (car input) keywords)
           (parse-pattern (cddr input) keywords))
         (pattern-pair
           (parse-pattern (car input) keywords)
           (parse-pattern (cdr input) keywords)))]
    [else (pattern-constant input)]))

; PATTERN DESTRUCTIONS
;
; (a ...)
; (1 2 3)
; a: (ellipsis 1 2 3)
;
; ((a b) ...)
; ((a 1) (b 2) (c 3))
; a: (ellipsis a b c)
; b: (ellipsis 1 2 3)
;
; ((a ...) ...)
; ((1 2 3) (a b c) (#\a #\b #\c))
; a: (ellipsis (ellipsis 1 2 3) (ellipsis a b c) (ellipsis #\a #\b #\c))

(define (pattern->destructor pattern)
  (cases pattern
    (pattern-variable (name)
      (variable-destructor name))
    (pattern-ellipsis (subpattern restpattern)
      (ellipsis-destructor subpattern restpattern))
    (pattern-pair (a d)
      (pair-destructor a d))
    (pattern-constant (value)
      (constant-destructor value))
    ))


(define (variable-destructor name)
  (lambda (input)
    (list (cons name input))))

(define (ellipsis-destructor subpattern restpattern)
  (let ([subdestructor (pattern->destructor subpattern)]
        [restdestructor (pattern->destructor restpattern)])
    (lambda (input)
      (let loop ([structure '()]
                 [input input])
        (cond
          [(pair? input)
           (cond
             [(subdestructor (car input))
              => (lambda (substructure)
                   (loop (add-structure structure substructure)
                         (cdr input)))]
             [(restdestructor input)
              => (lambda (reststructure)
                   (append reststructure structure))]
             [else #f])]
          [(restdestructor input)
           => (lambda (reststructure)
                (append reststructure structure))]
          [else #f])))))

(define (pair-destructor a d)
  (let ([cardestructor (pattern->destructor a)]
        [cdrdestructor (pattern->destructor d)])
    (lambda (input)
      (and (pair? input)
           (let ([carstructure (cardestructor (car input))]
                 [cdrstructure (cdrdestructor (cdr input))])
             (and carstructure
                  cdrstructure
                  (append carstructure
                          cdrstructure)))))))

(define (constant-destructor const)
  (lambda (input)
    (and (equal? input const)
         '())))




(define (add-structure structure substructure)
  (if (null? structure)
      (map (lambda (var)
             (let ([name (car var)]
                   [val (cdr var)])
               (cons* name 'ellipsis (list val))))
           substructure)
      (map (lambda (var)
             (let ([name (car var)]
                   [val (cddr var)])
               (cond
                 [(assq name substructure)
                  => (lambda (subvar)
                       (cons* name
                              'ellipsis
                              (append val (list (cdr subvar)))))]
                 [else var])))
           structure)))




;;;;;;;;;;;; TESTING ;;;;;;;;;;;;

(define (destruct pat)
  (pattern->destructor
    (parse-pattern pat '())))
