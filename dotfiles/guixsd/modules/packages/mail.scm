(define-module (packages mail)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public exim-lmtp
  (package
    (inherit exim)
    (name "exim-lmtp")
    (arguments
     (substitute-keyword-arguments (package-arguments exim)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'configure 'enable-lmtp
              (lambda _
                (substitute* "Local/Makefile"
                  (("# (TRANSPORT_LMTP=yes)" all line) line))))))))))
