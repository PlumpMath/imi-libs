#!r6rs

(library (imi lib parser)
  (export parse-lib)
  (import (rnrs)
          (imi list alist)
          (imi match extended))

  (define lib-matcher
    (extended-matcher
      '('library library-name
         ('export . exports)
         ('import . imports)
         
         . body)))

  (define (parse-lib lib)
    (let ([lib-matched (lib-matcher lib)])
      (if lib-matched
          (let ([match (alist->getq lib-matched)])
            (values (match 'library-name)
                    (parse-exports
                      (match 'exports))
                    (parse-imports
                      (match 'imports))
                    (match 'body)))
          (error 'parse-lib
                 "invalid library form"
                 lib))))

  (define (parse-exports exports)
    (apply append
           (map parse-export exports)))

  (define export-rename-matcher
    (extended-matcher
      '('rename . renames)))

  (define (parse-export export)
    (cond
      [(export-rename-matcher export)
       => (lambda (matches)
            (map (lambda (rename)
                   (list (cadr rename)
                         (cons 'rename-from
                               (car rename))))
                 (cdr (assq 'renames matches))))]
      [else
       (list (list export))]))


  (define (parse-imports imports)
    (map parse-import imports))

  (define import-library-matcher
    (extended-matcher
      '(? ('library library)
          library)))

  (define import-only-matcher
    (extended-matcher
      '('only import . identifiers)))

  (define import-except-matcher
    (extended-matcher
      '('except import . identifiers)))

  (define import-prefix-matcher
    (extended-matcher
      '('prefix import prefix)))

  (define import-rename-matcher
    (extended-matcher
      '('rename import . renames)))

  
  (define (parse-import import)
    (cond
      [(import-rename-matcher import)
       => import-rename]
      [(import-prefix-matcher import)
       => import-prefix]
      [(import-except-matcher import)
       => import-except]
      [(import-only-matcher import)
       => import-only]
      [(import-library-matcher import)
       => import-library]
      [else
       (error 'parse-import
              "invalid import form"
              import)]))

  (define (import-rename alist)
    (define get (alist->getq alist))

    (append (parse-import (get 'import))
            (map (lambda (rename)
                   (cons 'rename rename))
                 (get 'renames))))

  (define (import-prefix alist)
    (define get (alist->getq alist))

    (append (parse-import (get 'import))
            (list (cons 'prefix (get 'prefix)))))

  (define (import-except alist)
    (define get (alist->getq alist))

    (append (parse-import (get 'import))
            (list (cons 'except (get 'identifiers)))))

  (define (import-only alist)
    (define get (alist->getq alist))

    (append (parse-import (get 'import))
            (list (cons 'only (get 'identifiers)))))

  (define (import-library alist)
    (list (cdr (assq 'library alist))))
  

  )

