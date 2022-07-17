(define-module (packages linux)
  #:use-module (nongnu packages linux)
  #:use-module (gnu packages linux))

(define-public linux-5.13
  (corrupt-linux linux-libre-5.13 "5.13.16"
                 "1ljigvcg4q6ckr8kna3q5iyjsy7x5mrf1ycqfy0ibbhn9hbqjna9"))
