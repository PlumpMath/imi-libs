(define-alias *session*     variable "$_SESSION")
(define-alias *sid*         variable "SID")

(define-alias session-start          function "session_start")
(define-alias session-register       function "session_register")
(define-alias session-unregister     function "session_unregister")
(define-alias session-registered?    function "session_is_registered")
