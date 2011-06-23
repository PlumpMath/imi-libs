#!r6rs

(library (imi io compat filesystem)
  (export directory-list
          file-directory?
          (rename (file-exists? path-exists?)))
  (import (only (ikarus)
                directory-list
                file-directory?
                file-exists?))

  )

