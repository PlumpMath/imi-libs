#!r6rs

(library (imi prog mpd control)
  (export mpd-play
          mpd-play-in-playlist
          mpd-play-song

          mpd-stop

          mpd-pause

          mpd-previous
          mpd-next
          
          mpd-seek-in-playlist
          mpd-seek-song)
  (import (rnrs)
          (imi sugar rec)
          (imi utils print)
          (imi prog mpd base)
          (imi prog mpd utils))

  (define-mpd-cmd (mpd-play)
    (print-to cmd "play")
    (handle-mpd-return mpd-conn (get-line ret)))

  (define-mpd-cmd (mpd-play-in-playlist song)
    (print-to cmd "play" song)
    (handle-mpd-return mpd-conn (get-line ret)))

  (define-mpd-cmd (mpd-play-song songid)
    (print-to cmd "playid" songid)
    (handle-mpd-return mpd-conn (get-line ret)))


  (define-mpd-cmd (mpd-stop)
    (print-to cmd "stop")
    (handle-mpd-return mpd-conn (get-line ret)))


  (define-mpd-cmd (mpd-pause pause?)
    (print-to cmd "pause" (if pause? 1 0))
    (handle-mpd-return mpd-conn (get-line ret)))

  
  (define-mpd-cmd (mpd-next)
    (print-to cmd "next")
    (handle-mpd-return mpd-conn (get-line ret)))

  (define-mpd-cmd (mpd-previous)
    (print-to cmd "previous")
    (handle-mpd-return mpd-conn (get-line ret)))


  (define-mpd-cmd (mpd-seek-in-playlist song time)
    (print-to cmd "seek" song time)
    (handle-mpd-return mpd-conn (get-line ret)))

  (define-mpd-cmd (mpd-seek-song songid time)
    (print-to cmd "seekid" songid time)
    (handle-mpd-return mpd-conn (get-line ret)))

  )
