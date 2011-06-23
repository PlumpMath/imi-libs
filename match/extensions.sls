#!r6rs

(library (imi match extensions)
  (export make-simple-extension
          make-simple-extension/predicate
          make-simple-extension*

          match-quote
          match-symbols
          label-matches
          match-alternatives
          match-and
          match-procedures-simple)
  (import (rnrs)
          (imi list utils)
          (imi match simple-matchers)
          (imi match utils))


  ;;; creates a simple extension which has only one
  ;;; form and else matches the basic matcher given
  ;;; to the extension
  ;;; the form is given for `qm-match` (see (imi match
  ;;; utils))
  ;;; the extension-matcher given is a procedure which
  ;;; takes the pattern and the recursive matcher
  ;;; the extension produced conforms to the requirements
  ;;; given by `matcher-extend*` in (imi match extend)
  (define (make-simple-extension form extension-matcher)
    (lambda (pattern basic-matcher matcher)
      (cond
        [(qm-match form pattern)
         (extension-matcher pattern matcher)]
        [else
         (basic-matcher pattern matcher)])))


  ;;; this creates an extension like `make-simple-extension`,
  ;;; but instead of determining if a pattern is for this
  ;;; extension by a qm-match pattern it uses a predicate
  ;;; given
  ;;; for more information look at `make-simple-extension`
  (define (make-simple-extension/predicate pred extension-matcher)
    (lambda (pattern basic-matcher matcher)
      (cond
        [(pred pattern)
         (extension-matcher pattern matcher)]
        [else
         (basic-matcher pattern matcher)])))



  ;;; this creates a slightly different extension to 
  ;;; `make-simple-extension`, which takes a list of
  ;;; forms it matches the pattern and reports the
  ;;; extension-matcher first, which form is used, the
  ;;; other two arguments are the same
  ;;; for more information look at `make-simple-extension`
  (define (make-simple-extension* forms extension-matcher)
    (lambda (pattern basic-matcher matcher)
      (cond
        [(find (lambda (form)
                 (qm-match form pattern))
               forms)
         => (lambda (matched-form)
              (extension-matcher matched-form pattern matcher))]
        [else
         (basic-matcher pattern matcher)])))






  ;;; matches an expression without parsing it
  ;;; if it is quoted - so you can define constant
  ;;; symbols which are keywords instead of patterns
  ;;; matching everything and putting the expression
  ;;; into the match
  ;;; 
  ;;; form:
  ;;;   'sth
  ;;;   (quote sth)
  ;;;
  ;;; vars:
  ;;;   sth  - will not be parsed
  (define match-quote
    (make-simple-extension
      '(quote ?)
      (lambda (pattern matcher)
        (equal-matcher (cadr pattern) matcher))))



  ;;; overrides the standard handling of symbols,
  ;;; which don't compare in the `basic-matcher*`
  ;;; but only mark something for the output
  ;;; by this extension symbols will compare by
  ;;; default
  (define match-symbols
    (make-simple-extension/predicate
      symbol?
      (lambda (symb matcher)
        (lambda (expr)
          (and (symbol=? expr symb)
               '())))))



  ;;; puts a specific matching expression into the
  ;;; resulting matches
  ;;;
  ;;; form:
  ;;;   (label name sth*)
  ;;;   (:     name sth*)
  ;;;
  ;;; vars:
  ;;;   name  - the name of the match in the output
  ;;;   sth*  - optional: the pattern which has to match
  ;;;           if without sth it matches everything
  (define label-matches
    (make-simple-extension*
      (list '(label ? ?) '(: ? ?)
            '(label ?)   '(: ?))
      (lambda (form pattern matcher)
        (let ([label (cadr pattern)]
              [pattern-matcher
                (if (length=? pattern 2)
                    (lambda (expr) '())
                    (matcher (caddr pattern) matcher))])
          (lambda (expr)
            (cond
              [(pattern-matcher expr)
               => (lambda (result)
                    (cons (cons label expr)
                          result))]
              [else #f]))))))




  ;;; matches alternatives; tries the alternatives
  ;;; in given order from left to right
  ;;;
  ;;; form:
  ;;;   (or alt ...)
  ;;;   (/  alt ...)
  ;;;
  ;;; vars:
  ;;;   alt ...  - the alternatives
  (define match-alternatives
    (make-simple-extension*
      (list '(or ? . ?)
            '(/  ? . ?))
      (lambda (form pattern matcher)
        (let ([matchers (map (lambda (subpattern)
                               (matcher subpattern matcher))
                             (cdr pattern))])
          (lambda (expr)
            (exists (lambda (matcher)
                      (matcher expr))
                    matchers))))))



  ;;; combines many patterns for one expression, so
  ;;; all patterns have to match to match the whole
  ;;; and-pattern (this makes sense for example in
  ;;; combination with `match-procedures-simple`)
  ;;;
  ;;; form:
  ;;;   (and patterns ...)
  ;;;
  ;;; vars:
  ;;;   patterns ...   - the patterns which have to
  ;;;                    match
  ;;;                    can also have no patterns
  ;;;                    so it matches to everything
  (define match-and
    (make-simple-extension
      '(and . ?)
      (lambda (pattern matcher)
        (let ([matchers (map (lambda (subpattern)
                               (matcher subpattern matcher))
                             (cdr pattern))])
          (lambda (expr)
            (fold-left (lambda (results matcher)
                         (cond
                           [(not results) #f]
                           [(matcher expr)
                            => (lambda (res)
                                 (append res results))]
                           [else #f]))
                       '() ;results: lists - matches; #f - no match
                       matchers))))))



  ;;; simplifies the procedure matching: instead of
  ;;; letting a procedure in the pattern be another
  ;;; matcher-creator a procedure should just be a
  ;;; predicate which matches an expression if it
  ;;; evaluates to true, else it doesn't match
  ;;;
  ;;; this concept takes away much of the power of
  ;;; procedures in a pattern, but simplifies the
  ;;; use of some things like ``(a ,list? b)`
  ;;;
  ;;; form:
  ;;;   procedure
  ;;;
  ;;; vars:
  ;;;   procedure - the predicate procedure
  (define match-procedures-simple
    (make-simple-extension/predicate
      procedure?
      (lambda (predicate matcher)
        (lambda (expr)
          (and (predicate expr)
               '())))))


  )

