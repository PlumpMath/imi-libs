(library (imi asm core bytes)
  (export bits-set
          bits-set-from
          mask
          mask-out
          overwrite
          bits-in-range
          bits-move-to-range
          bits-edit-range
          edit-masked)
  (import (rnrs))

  ;;; creates an integer with bits set
  ;;;   from bit 0 to bit `len`-1
  ;;;
  ;;; len - integer?
  ;;;  -> integer?
  (define (bits-set len)
    (- (bitwise-arithmetic-shift 1 len)
       1))

  ;;; creates an integer with bits set
  ;;;   from bit `start` to bit `end`
  ;;;
  ;;; start - integer?
  ;;; end - integer?
  ;;;  -> integer?
  (define (bits-set-from start end)
    (bitwise-arithmetic-shift
      (bits-set (- end start))
      start))

  ;;; masks a range of bits from `start`
  ;;;   to `end`
  ;;;
  ;;; bytes - integer?
  ;;; start - integer?
  ;;; end - integer?
  ;;;  -> integer?
  (define (mask bytes start end)
    (bitwise-and
      bytes
      (bits-set-from start end)))

  ;;; masks a range of bits out (set to 0)
  ;;;
  ;;; bytes - integer?
  ;;; start - integer?
  ;;; end - integer?
  ;;;  -> integer?
  (define (mask-out bytes start end)
    (bitwise-and
      bytes
      (bitwise-not (bits-set-from start end))))

  ;;; overwrites bits in `bytes` from `start` to
  ;;;   `end` with bits in `over` from 
  ;;;   `start` to `end`
  ;;;
  ;;; bytes - integer?
  ;;; over - integer?
  ;;; start - integer?
  ;;; end - integer?
  ;;;  -> integer?
  (define (overwrite bytes over start end)
    (bitwise-ior
      (mask over start end)
      (mask-out bytes start end)))

  ;;; creates an integer with the bits
  ;;;   from `start` to `end` in bytes,
  ;;;   which will appear in same order
  ;;;   at the start (from bit 0 on)
  ;;;
  ;;; bytes - integer?
  ;;; start - integer?
  ;;; end - integer?
  ;;;  -> integer?
  (define (bits-in-range bytes start end)
    (bitwise-arithmetic-shift (mask bytes start end)
                              (- start)))

  ;;; creates an integer with the bits
  ;;;   from 0 to `end`-`start` at
  ;;;   positions `start` to `end`
  ;;;
  ;;; bytes - integer?
  ;;; start - integer?
  ;;; end - integer?
  ;;;  -> integer?
  (define (bits-move-to-range bytes start end)
    (mask (bitwise-arithmetic-shift bytes start)
          start
          end))
  
  ;;; edits bits in a specific range and
  ;;;   sets the rest to 0. The bits will
  ;;;   be shifted to start from pos 0
  ;;;
  ;;; bytes - integer?
  ;;; start - integer?
  ;;; end - integer?
  ;;; proc - (-> integer? integer?)
  ;;;  -> integer?
  (define (bits-edit-range bytes start end proc)
    (bits-move-to-range
      (proc (bits-in-range bytes start end))
      start
      end))

  ;;; edits only a specific range of bits
  ;;;   which are shifted to begin from
  ;;;   bit 0 to edit and after editing
  ;;;   shifted back to their position
  ;;;
  ;;; bytes - integer?
  ;;; range-from - integer?
  ;;; range-to - integer?
  ;;; proc - (-> integer? integer?)
  ;;;  -> integer?
  (define (edit-masked bytes range-from range-to proc)
    (overwrite
      bytes
      (bits-edit-range bytes range-from range-to proc)
      range-from
      range-to))

  ;;; edits only specific bits which are given
  ;;;   by the mask
  ;;;
  ;;; bytes - integer?
  ;;; mask - integer?
  ;;; proc - (-> integer? integer?)
  ;;;  -> integer?
  (define (edit-with-mask bytes mask proc)
    (bitwise-ior
      (bitwise-and
        (proc (bitwise-and bytes mask))
        mask)
      (bitwise-and
        bytes
        (bitwise-not bytes))))

  )
