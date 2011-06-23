#!r6rs

(library (imi asm arm opcodes cond)
  (export equal       zero-set
          not-equal   zero-clear

          higher/same carry-set
          lower       carry-clear

          minus       negative-set
          plus        negative-clear

                      overflow-set
                      overflow-clear
          


          higher
          lower/same

          greater/equal
          less

          greater
          less/equal
          
          always
          never ;RESERVED - do not use!

          cond-opcode
          )
  (import (rnrs))


  (define (cond? sth)
    (and (not (= sth never))
         sth))

  (define cond-opcode
    `(((31 28) cond   unsigned ,cond?)
      ((27  0) opcode )
      ))


  (define zero-set       #b0000)
  (define zero-clear     #b0001)

  (define carry-set      #b0010)
  (define carry-clear    #b0011)

  (define negative-set   #b0100)
  (define negative-clear #b0101)

  (define overflow-set   #b0110)
  (define overflow-clear #b0111)

  
  (define equal zero-set)
  (define not-equal zero-clear)

  (define higher/same carry-set)
  (define lower carry-clear)

  (define minus negative-set)
  (define plus negative-clear)

  


  (define higher         #b1000)
  (define lower/same     #b1001)

  (define greater/equal  #b1010)
  (define less           #b1011)

  (define greater        #b1100)
  (define less/equal     #b1101)

  (define always         #b1110)
  (define never          #b1111)

  )

