(library (imi proc channel)
  (export call-with-channel
          channel-put
          channel-end)
  (import (rnrs)
          (imi iter iterator)
          (imi sugar continuation))

  ;;; creates a channel to communicate
  ;;;  to the calling procedure
  ;;;
  ;;; proc - (-> ret channel)
  ;;;  -> ret
  (define (call-with-channel proc)
    (let/cc channel
      (proc channel)))

  ;;; puts a value into the channel,
  ;;;  serving it to the calling procedure,
  ;;;  returning the new channel
  ;;;
  ;;; channel - channel
  ;;; val - any?
  ;;;  -> channel
  (define (channel-put channel val)
    (let/cc back
      (channel
        (make-iterator
          val
          (lambda ()
            (call/cc back))))))

  ;;; "closes" the channel - never
  ;;;  returns (directly returns to the
  ;;;  last call to get the next value
  ;;;  of the iterator)
  ;;;
  ;;; channel - channel
  (define (channel-end channel)
    (channel #f))

  )
