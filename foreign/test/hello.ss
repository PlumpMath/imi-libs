#!r6rs

(library (imi foreign test hello)
  (export greet

          make-point
          point->pointer
          pointer->point

          point-x
          point-x-set!

          point-y
          point-y-set!)
  (import (rnrs)
          (imi foreign import-c-library))

  (import-c-library (imi foreign test libhello)
    ; void greet ()
    (void greet ())

    (struct point
      (uint x)
      (uint y))

    )

  )
