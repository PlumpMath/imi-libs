#!r6rs

(library (imi asm opcodes)
  (export opcode-form)
  (import (except (rnrs)
                  bitwise-copy-bit-field)
          (only (imi math bitwise bit-field)
                bitwise-copy-bit-field)
          (imi sugar cut)
          (imi sugar receive)
          (imi list utils)
          (imi string format)
          (imi asm utils))

  (define (make-opcode desc)
    (fold-left (lambda (opcode opcode-part)
                 (case-match opcode-part
                   [((: from) (: to) (: what ,number?))
                    (from to what)
                    (bitwise-copy-bit-field opcode from to what)]
                   [else () (error 'make-opcode
                                   "invalid opcode part"
                                   opcode-part)]))
               0
               (map parse-opcode-column
                    (desc->columns desc))))

  (define (desc->columns desc)
    (case-match desc
      [((: pos) (: value) (: pred))
       (pos value pred)
       (map list pos value pred)]
      [else () (error 'desc->columns
                      "not a valid opcode description"
                      desc)]))

  (define (parse-opcode-column column)
    (case-match column
      [((: range) (: description) (: pred #f))
       (range description pred)
       (receive (from to) (parse-range range)
         (list from
               to
               (parse-code from
                           to
                           description)))]
      [else () (error 'parse-opcode-column
                      "invalid column form"
                      column)]))

  (define (parse-range range)
    (case-match range
      [((: to ,number?) (: from ,number?))
       (to from)
       (values from
               (+ 1 to))]
      [(: bit ,number?)
       (bit)
       (values bit
               (+ 1 bit))]
      [else () (error 'parse-range
                      "invalid range form"
                      range)]))

  (define (parse-code from to desc)
    (receive (signed? num) (parse-desc desc)
      (let ([range-pred (make-range-check signed? from to)])
        (cond
          [(not (number? desc))
           (error 'parse-code
                  "opcode partially not filled in"
                  code)]
          [(not (range-pred code))
           (error 'parse-code
                  "invalid code - not in range"
                  desc)]
          [else desc]))))

  (define (parse-desc desc)
    (case-match desc
      [((: sign (or signed unsigned)) (: number ,number?))
       (sign number)
       (values (symbol=? sign 'signed)
               number)]
      [(: number ,number?)
       (number)
       (values #f number)]
      [else
       ()
       (error 'parse-desc
              "invalid description"
              desc)]))

  ;;; FIXME: is the (expt 2 x) stuff really right? ...
  (define (make-range-check signed? from to)
    (let ([bits (- to from)])
      (let ([range-min (if signed?
                           (- (expt 2 (- bits 1)))
                           0)]
            [range-max (if signed?
                           (expt 2 (- bits 1))
                           (expt 2 bits))])
        (lambda (num)
          (<= range-min
              num
              range-max)))))




  (define (opcode-form name form)
    (case-match form
      [((: pos) (: name) (: pred))
       (pos name pred)
       (let* ([opfields (map (field-processor name) pos name pred)]
              [start (position-start (last pos))]
              [fieldcount (length opfields)])
         (lambda args
           (if (length=? args fieldcount)
               (bitwise-arithmetic-shift-right
                 (opcode-combine
                   (map (cut <> <>) opfields args))
                 start)
               (error name
                      "wrong argument count"
                      args))))]
      [else
       ()
       (error 'opcode-form
              "wrong form description"
              form)]))

  
  (define (opcode-combine subcodes)
    (fold-left bitwise-ior
               0
               subcodes))


  (define (position-start pos)
    (case-match pos
      [(: start ,number?)
       (start)
       start]
      [((: end ,number?) (: start ,number?))
       (start)
       start]
      [else
       ()
       (error 'position-start
              "invalid position form"
              pos)]))


  (define (field-processor opcode-name)
    (lambda (pos name pred)
      (case-match pos
        [(: bit ,number?)
         (bit)
         (field-bit-processor opcode-name pos name pred)]
        [((: end ,number?) (: start ,number?))
         (end start)
         (field-range-processor opcode-name start end name pred)]
        [else
         ()
         (error 'field-processor
                "invalid position description"
                pos)])))

  (define (field-bit-processor opcode-name pos name pred)
    (lambda (bit)
      (cond
        [(number? bit)
         (if (<= 0 bit 1)
             (bitwise-arithmetic-shift-left bit pos)
             (error opcode-name
                    (format "~s is a bit, can only be 0, 1 or a boolean" name)
                    bit))]
        [(boolean? bit)
         (bitwise-arithmetic-shift-left
           (if bit 1 0)
           pos)]
        [else
         (error opcode-name
                (format "~s is a bit (~s), can only be a number or boolean"
                        name
                        pred)
                bit)])))


  (define (field-range-processor opcode-name start end name pred)
    (let ([check-val (value-check start
                                  (+ 1 end)
                                  pred)])
      (lambda (num)
        (if (check-val num)
            (bitwise-copy-bit-field 0 start end num)
            (error opcode-name
                   (format "invalid ~s, not in range or ~s"
                           name
                           "doesn't match other constraints")
                   num)))))

  (define (value-check start end pred)
    (receive (sign pred) (parse-pred pred)
      (let ([size (- end start)])
        (let ([maxval (get-maxval sign size)]
              [minval (get-minval sign size)])
          (lambda (val)
            (<= minval val maxval))))))


  (define (parse-pred pred)
    (case-match pred
      [(: pred ,pred?)
       (pred)
       (values 'unsigned
               (get-pred pred))]
      [(: sign ,symbol?)
       (sign)
       (values sign
               (get-pred 'all))]
      [((: sign ,symbol?) (: pred ,pred?))
       (sign pred)
       (values sign
               (get-pred pred))]
      [else
       ()
       (error 'parse-pred
              "invalid predicate form"
              pred)]))

  (define (get-maxval sign size)
    (case sign
      [(unsigned)
       (- (expt 2 size)
          1)]
      [(signed)
       (- (expt 2 (- size 1))
          1)]
      [else
       (error 'get-maxval
              "invalid sign"
              sign)]))

  (define (get-minval sign size)
    (case sign
      [(unsigned) 0]
      [(signed)
       (- (expt 2 (- size 1)))]
      [else
       (error 'get-minval
              "invalid sign"
              sign)]))


  (define (pred? sth)
    (or (procedure? sth)
        (eq? sth 'all)))

  (define (get-pred sth)
    (case-match sth
      [,procedure? () sth]
      [all () (lambda (x) #t)]
      [else
       ()
       (error 'get-pred
              "invalid predicate"
              sth)]))
  

  )

