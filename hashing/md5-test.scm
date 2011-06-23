#!/usr/bin/env scheme-script

#!r6rs

(import (rnrs)
        (imi sugar list-let)
        (imi hashing md5)
        (imi stream convert))

(define (string-md5 str)
  (md5 (list->stream
         (bytevector->u8-list
           (string->utf8 str)))))

(define (md5string=? a b)
  (= (string->number a 16)
     (string->number b 16)))

(define (test-md5 tests)
  (if (for-all values
               (map (lambda (test)
                      (list-let (teststr refres) test
                        (printf "testing ~s:~%" teststr)
                        (let* ([res (md5hash->string
                                      (string-md5 teststr))]
                               [passed? (md5string=? res refres)])
                          (printf "  result:    ~s~%" res)
                          (printf "  reference: ~s~%" refres)
                          (printf " ~a~%" (if passed?
                                            "PASSED"
                                            "FAILED!"))
                          (newline)
                          passed?)))
                    tests))
      (printf "all tests passed.")
      (printf "FAILURE!"))
  (newline))

(define (test-md5-silent tests)
  (for-all (lambda (test)
             (list-let (teststr refres) test
               (md5string=? refres
                            (md5hash->string
                              (string-md5 teststr)))))
           tests))

(define rfc-tests
  '((""               "d41d8cd98f00b204e9800998ecf8427e")
    ("a"              "0cc175b9c0f1b6a831c399e269772661")
    ("abc"            "900150983cd24fb0d6963f7d28e17f72")
    ("message digest" "f96b697d7cb7938d525a2f31aaf161d0")
    ("abcdefghijklmnopqrstuvwxyz"
                      "c3fcd3d76192e4007dfb496cca67e13b")
    ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
                      "d174ab98d277d9f5a5611c2c9f419d9f")
    ("12345678901234567890123456789012345678901234567890123456789012345678901234567890" 
                      "57edf4a22be3c955ac49da2e2107b67a")))

(define (test-rfc)
  (test-md5 rfc-tests))


(test-rfc)
