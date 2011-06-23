#!r6rs

(library (imi io file)
  (export directory-maprec
          directory-findrec
          directory-filterrec)
  (import (rnrs)
          (imi utils tester)
          (imi io filesystem))

  (define (directory-maprec dir-filter proc dir)
    (let-values ([(dirs files)
                  (partition file-directory?
                             (map (lambda (subdir)
                                    (string-append dir "/" subdir))
                                  (remp (test exists "." "..")
                                        (directory-list dir))))])
      (append (map proc files)
              (apply append
                     (map (lambda (ndir)
                            (directory-maprec dir-filter proc ndir))
                          (if dir-filter
                              (filter dir-filter dirs)
                              dirs))))))

  (define (directory-findrec dir-filter pred dir)
    (remp not (directory-maprec dir-filter
                                (lambda (file)
                                  (and (pred file)
                                       file))
                                dir)))

  (define (directory-filterrec dir-filter pred dir)
    (remp not (directory-maprec dir-filter pred dir)))

  )

