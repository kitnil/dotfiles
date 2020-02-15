(define-module (wigust packages docker)
  #:use-module (gnu packages docker)
  #:use-module (guix packages))

(define-public containerd-without-tests
  (package
    (inherit containerd)
    (arguments `(#:tests? #f ,@(package-arguments containerd)))))

(define-public my-docker
  (package
    (inherit docker)
    (inputs `(("containerd" ,containerd-without-tests)
              ,@(assoc-remove! (package-inputs docker) "containerd")))))
