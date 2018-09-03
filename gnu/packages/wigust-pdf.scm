(define-module (gnu packages wigust-pdf)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages pdf)
  #:use-module (guix build utils))

(define-public mupdf-dark-background
  (package
    (inherit mupdf)
    (name "mupdf-dark-background")
    (source
     (origin
       (inherit (package-source mupdf))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; TODO: Inherit snippet from mupdf
           (delete-file-recursively "thirdparty")

           ;; Change color from #777777 #000000
           (substitute* "platform/x11/x11_main.c"
             (("0x7000") "0x0000"))))))))
