(library (imi string processing)
  (export string-remove
          string-chomp
          string-split
          string-contains?)
  (import (rnrs)
          (imi sugar cut)
          (imi list processing))

  ;;; pendant to remove in lists
  ;;;   removes char in string
  ;;;
  ;;; char - character?
  ;;; str - string?
  ;;;  -> string?
  (define (string-remove char str)
    (list->string
      (remove char
              (string->list str))))

  ;;; removes :TODO:
  (define (string-chomp proc str)
    (list->string
      (reverse
        (list-chomp 
          proc
          (reverse
            (list-chomp
              proc
              (string->list str)))))))

  (define (string-split split? str)
    (map list->string
         (remp null?
               (list-split split?
                           (string->list str)))))


  (define (string-contains? str0 str1)
    (list-contains char=?
                   (string->list str0)
                   (string->list str1)))

  )
