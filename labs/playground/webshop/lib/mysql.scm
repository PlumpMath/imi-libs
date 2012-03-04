(define-alias *mysql-use-result* variable "MYSQLI_USE_RESULT")

(define-alias *mysql-both*       variable "MYSQLI_BOTH")
(define-alias *mysql-num*        variable "MYSQLI_NUM")
(define-alias *mysql-assoc*      variable "MYSQLI_ASSOC")


(define-alias mysql-connect       function "mysqli_connect")
(define-alias mysql-close         function "mysqli_close")

(define-alias mysql-query         function "mysqli_query")
(define-alias mysql-free-result   function "mysqli_free_result")
(define-alias mysql-num-rows      function "mysqli_num_rows")
(define-alias mysql-fetch         function "mysqli_fetch_array")
(define-alias mysql-fetch-assoc   function "mysqli_fetch_assoc")
(define-alias mysql-fetch-row     function "mysqli_fetch_row")
(define-alias mysql-fetch-object  function "mysqli_fetch_object")


(define-alias mysql-error         function "mysqli_error")
(define-alias mysql-connect-errno function "mysqli_connect_errno")
(define-alias mysql-connect-error function "mysqli_connect_error")
