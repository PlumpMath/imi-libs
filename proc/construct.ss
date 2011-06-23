#!r6rs

(library (imi proc construct)
  (export construct
          construct*
          construct-list
          construct-list*)
  (import (rnrs))

  ;;; constructs something by combining the new value and
  ;;;   the rest with `combine`, whereby the rest is end
  ;;;   if applying `finished?` on the new value is true.
  ;;;   the new value is computed by proc on the old
  ;;;   value, which is on the beginning start
  ;;;
  ;;; resulttype - (or/c (combine/c valuetype resulttype)
  ;;;                    endtype)
  ;;;
  ;;; combine - (-> resulttype
  ;;;               valuetype
  ;;;               resulttype)
  ;;; end - endtype
  ;;; start - valuetype
  ;;; proc - (-> valuetype valuetype)
  ;;; finished? - (-> any? valuetype)
  ;;;  -> resulttype
  (define (construct combine end start proc finished?)
    (let loop ([value start])
      (if (finished? value)
          end
          (combine value
                   (loop (proc value))))))

  (define (construct* combine end proc finished?)
    (construct combine
               end
               #f
               (lambda (ignore-old)
                 (proc))
               finished?))

  ;;; constructs a list like construct with cons as combine
  ;;;   and null as the end
  ;;;
  ;;; start - valuetype
  ;;; proc - (-> valuetype valuetype)
  ;;; finished? - (-> any? valuetype)
  ;;;  -> (listof/c valuetype)
  (define (construct-list start proc finished?)
    (construct cons '() start proc finished?))

  (define (construct-list* proc finished?)
    (construct* cons '() proc finished?))

  )
