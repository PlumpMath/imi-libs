(library (imi time)
  (export get-timeinfo

          timeinfo-time
          timeinfo-date
          timeinfo-timezone

          time-hour
          time-minute
          time-second

          date-month
          date-day
          date-year

          timezone-spec
          timezone-name)
  (import (rnrs)
          (imi foreign process))

  (define (get-timeinfo)
    (let-values ([(pid stdin stdout stderr)
                  (process "date" "+((%H %M %S) (%m %d %Y) (\"%z\" \"%Z\"))")])
      (read (transcoded-port stdout (native-transcoder)))))

  (define (timeinfo-time ti) (car ti))
  (define (timeinfo-date ti) (cadr ti))
  (define (timeinfo-timezone ti) (caddr ti))
  
  (define (time-hour t) (car t))
  (define (time-minute t) (cadr t))
  (define (time-second t) (caddr t))

  (define (date-month d) (car d))
  (define (date-day d) (cadr d))
  (define (date-year d) (caddr d))

  (define (timezone-spec tz) (car tz))
  (define (timezone-name tz) (cadr tz))

  )
