(library (imi foreign c-loader)
  (export open-c-library)
  (import (rnrs)
          (imi io path)
          (imi lib)
          (imi foreign c-library))

  (define (open-c-library spec)
    (let ([paths-to-search
            (map (lambda (libpath)
                   (path->string
                     (path-append libpath spec)))
                 (library-path))])
      (dlopen
        (or (find file-exists? paths-to-search)
            (error 'open-c-library
                   "could not locate shared object in library path"
                   paths-to-search)))))

  )
