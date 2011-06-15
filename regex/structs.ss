(library (imi regex structs)
  (export make-match
          match?
          match-content
          match-position

          make-position
          position?
          position-start
          position-end)

  (import (rnrs))

  (define-record-type match
    (fields content position)
    (protocol
      (lambda (new)
        (lambda (cont pos)
          (unless (position? pos)
            (error 'make-match "must be a position" pos))
          (new cont pos)))))

  (define-record-type position
    (fields start end)
    (protocol
      (lambda (new)
        (lambda (start end)
          (unless (and (integer? start)
                       (integer? end))
            (error 'make-position "must be integers" start end))
          (new start end)))))

  )
