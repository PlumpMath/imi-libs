#!/usr/bin/env scheme-script

#!r6rs

(import (rnrs)
        (srfi :37)
        (match)
        (imi sugar cut)
        (imi sugar receive)
        (imi utils print)
        (imi io string)
        (imi list set)
        (imi list processing)
        (imi string processing)
        (imi string format)
        (imi string utils)
        )


(define (console-prompt-interaction prompt reader)
  (lambda (glue)
    (lambda init
      (let loop ([state (apply glue init)])
        (display prompt)
        (cond
          [(state (reader))
           => loop])))))

(define simple-console-prompt-interaction
  (console-prompt-interaction
    "> "
    (lambda ()
      (let ([input (get-line (current-input-port))])
        (if (eof-object? input)
            (begin (newline)
                   input)
            (with-input-from-string
              (string-append "(" input "\n)")
              read))))))



(define options
  (list))

(define (dependency-start args)
  (receive (filename)
    (args-fold args
               options
               (lambda (option name arg . seeds)
                 (error 'dependency
                        "unrecognized option"
                        name))
               (lambda (operand file)
                 (if file
                     (error 'dependency
                            "file already given"
                            file operand)
                     operand))
               #f)
    (if (not filename)
        (error 'dependency
               "no file given")
        (dependency-main-start filename))))


(define (dependency-main-start file)
  (dependency-main-console-interface
    (if (file-exists? file)
        (with-input-from-file file read)
        '())
    (lambda (deps)
      (and (file-exists? file)
           (delete-file file))
      (with-output-to-file file
        (lambda ()
          (write deps))))))



(define (dependency-main-interface deps output . cmd)
  (match cmd
    [(add ,obj . ,new-deps)
     (dependencies-add obj new-deps deps)]
    [(del ,obj)
     (dependencies-del obj deps)]
    [(del ,obj . ,del-deps)
     (dependencies-remove obj del-deps deps)]
    [(show)
     (output 'all-deps deps)
     deps]
    [(show ,obj)
     (output 'dep (dependencies-ref obj deps))
     deps]
    [(show - base)
     (output 'base (dependencies-base deps))
     deps]
    [(show - target)
     (output 'target (dependencies-target deps))
     deps]
    [,cmd
     (output 'invalid-cmd cmd)
     deps]
    ))


(define (dependencies-add obj new-deps deps)
  (cond
    [(assq obj deps)
     => (lambda (dep)
          (cons (cons obj
                      (list-union
                        eq?
                        new-deps
                        (cdr dep)))
                (dependencies-del dep deps)))]
    [else
     (cons (cons obj new-deps)
           deps)]))


(define (dependencies-del obj deps)
  (remp (lambda (dep)
          (eq? obj (car dep)))
        deps))


(define (dependencies-remove obj del-deps deps)
  (cond
    [(assq obj deps)
     => (lambda (dep)
          (cons (cons obj (remp (lambda (dep)
                                  (memq dep del-deps))
                                (cdr dep)))
                (dependencies-del obj deps)))]
    [else deps]))


(define (dependencies-ref obj deps)
  (assq obj deps))


(define (dependencies-base deps)
  (map car (filter (lambda (dep)
                     (null? (cdr dep)))
                   deps)))


(define (dependencies-target deps)
  (map car (filter (lambda (dep)
                     (null? (cdr dep)))
                   (dependencies->dependers deps))))



(define (dependency-console-glue-main deps return-deps)
  (let loop ([deps deps])
    (lambda (input)
      (cond
        [(or (eof-object? input)
             (equal? input '(quit)))
         (return-deps deps)
         #f]
        [(null? input)
         (loop deps)]
        [else
         (loop (apply dependency-main-interface
                      deps
                      (lambda (type obj)
                        (case type
                          [(all-deps) (show-all-deps obj)]
                          [(dep) (show-dep obj)]
                          [(base) (show-base obj)]
                          [(target) (show-target obj)]
                          [(invalid-cmd) (show-invalid-cmd obj)]
                          [else (error 'dependency-console-glue-main
                                       "invalid output type"
                                       type)]))
                      input))]))))



(define (show-all-deps deps)
  (print "All current dependencies:")
  (let loop ([deps deps]
             [base '()])

    (define (fulfilled? dep)
      (for-all (lambda (dep)
                 (memq dep base))
               (cdr dep)))

    (unless (null? deps)
      (receive (fulfilled rest) (partition fulfilled? deps)
        (if (null? fulfilled)
            (apply print "Error: cyclic or missing dependencies:" rest)
            (let ([fulfilled (map car fulfilled)])
              (apply print " " fulfilled)
              (loop rest
                    (append fulfilled base))))))))


(define (show-dep dep)
  (let* ([dep-prompt (format "~s:" (car dep))]
         [indentation (make-string (string-length dep-prompt)
                                   #\space)])
    (cond
      [(null? (cdr dep))
       (print dep-prompt "no dependencies")]
      [else
       (print (format "~s:" (car dep))
              (cadr dep))
       (for-each (lambda (dep)
                   (print indentation dep))
                 (cddr dep))])))


(define (show-base base)
  (cond
    [(null? base)
     (print "No base")]
    [else
     (print "base:" (car base))
     (for-each (lambda (base)
                 (print "     " base))
               (cdr base))]))


(define (show-target target)
  (cond
    [(null? target)
     (print "No target")]
    [else
     (print "target:" (car target))
     (for-each (lambda (target)
                 (print "       " target))
               (cdr target))]))


(define (show-invalid-cmd cmd)
  (apply print "Error - invalid command:" cmd))




(define dependency-main-console-interface
  (simple-console-prompt-interaction 
    dependency-console-glue-main))



(define (dependencies->dependers deps)
  (map (lambda (dep)
         (cons (car dep)
               (map car (filter (lambda (x)
                                  (memq (car dep)
                                        (cdr x)))
                                deps))))
       deps))


(dependency-start (cdr (command-line)))
