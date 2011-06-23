#!r6rs

(library (imi iter construct)
  (export iter-construct
          iter-construct-list)
  (import (rnrs)
          (imi sugar for))

  ;;; constructs something like construct
  ;;;  in (imi proc construct), but uses
  ;;;  an iterator to get the values
  ;;;
  ;;; resulttype - (or/c (combine/c valuetype resulttype)
  ;;;                    endtype)
  ;;;
  ;;; combine - (-> resulttype valuetype resulttype)
  ;;; end - endtype
  ;;; iter - (iterator/c valuetype)
  ;;;  -> resulttype
  (define (iter-construct combine end iter)
    (for loop ([value in iter])
         end
      (combine value (loop))))

  ;;; constructs a list of all values in
  ;;;   iter; uses iter-construct, like
  ;;;   construct-list for construct
  ;;;   in (imi proc construct)
  ;;;
  ;;; iter - (iterator/c valuetype)
  ;;;  -> (listof/c valuetype)
  (define (iter-construct-list iter)
    (iter-construct cons '() iter))

  )
