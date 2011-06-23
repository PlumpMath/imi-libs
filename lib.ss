#!r6rs

(library (imi lib)
  (export library-path

          lib->base-path
          base-path->lib

          fetch-library

          find-library
          find-in-library-path

          standard-library?
          
          filename->lib
          *scheme-file-endings*
          *implementation-file-endings*
          *scheme-suffixes*)
  (import (rnrs)
          (imi io filesystem)
          (imi implementation)
          (imi lib compat)
          (imi lib parser)
          (imi sugar cut)
          (imi sugar and-let)
          (imi list processing))

  (define (lib->base-path lib)
    (apply string-append
           (list-intersperse
             "/"
             (map symbol->string lib))))

  (define (base-path->lib path)
    (map string->symbol
         (map list->string
              (list-split (cut char=? #\/ <>)
                          (string->list path)))))

  (define (fetch-library lib)
    (if (standard-library? lib)
        (values lib
                (standard-library-exports lib)
                '()
                '())
        (parse-lib
          (with-input-from-file
            (or (find-library lib)
                (error 'fetch-library
                       "could not find library"
                       lib))
            read))))

  (define (find-in-library-path proc)
    (exists (lambda (path)
              (proc (lambda (file)
                      (let ([full (string-append path "/" file)])
                        (and (path-exists? full)
                             full)))))
            (filter path-exists? (library-path))))

  (define (find-library lib)
    (let ([lib-path (lib->base-path lib)])
      (find-in-library-path
        (lambda (find-file)
          (exists (lambda (suffix)
                    (find-file
                      (string-append lib-path
                                     suffix)))
                  *scheme-suffixes*)))))

  (define (filename->lib filename)
    (exists (lambda (suffix)
              (and-let
                [lib-file (list-match-start
                            char=?
                            (reverse (string->list suffix))
                            (reverse (string->list filename)))]
                (base-path->lib
                  (list->string (reverse lib-file)))))
            ;to check against the longest paths first reverse
            (reverse *scheme-suffixes*)))


  (define *scheme-file-endings*
    '(".sls" ".ss" ".scm"))

  (define *implementation-file-endings*
    (if implementation-name
        (append *scheme-file-endings*
                (map (lambda (end)
                       (string-append
                         "."
                         (symbol->string implementation-name)
                         end))
                     *scheme-file-endings*))
        *scheme-file-endings*))


  (define *scheme-suffixes*
    (append *implementation-file-endings*
            (map (lambda (ending)
                   (string-append "/main"
                                  ending))
                 *implementation-file-endings*)))

  )
