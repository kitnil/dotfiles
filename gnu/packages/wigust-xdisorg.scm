(define-module (gnu packages wigust-xdisorg)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages xdisorg)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:))

(define-public wrapper-xclip
  (let ((commit "38c9971b8ea380d8eac6543a8ca5c487dcc413ee"))
    (package
      (name "wrapper-xclip")
      (version (git-version "0.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/wigust/wrapper-xclip")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1yw6m47kajbzjlyspsihr6cydp3l9wj020gndkd2da9xb03c6vy8"))))
      (build-system trivial-build-system)
      (arguments
       '(#:modules
         ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (setenv "PATH"
                   (string-append
                    (assoc-ref %build-inputs "bash") "/bin" ":"
                    (assoc-ref %build-inputs "xclip") "/bin"))
           (copy-recursively (assoc-ref %build-inputs "source") ".")
           (for-each (lambda (file)
                       (substitute* file
                         (("/bin/sh") (which "bash"))
                         (("@XCLIP_BIN@") (which "xclip")))
                       (install-file file
                                     (string-append %output "/bin")))
                     '("xcopy" "xpaste")))))
      (inputs
       `(("bash" ,bash)
         ("xclip" ,xclip)))
      (synopsis "Wrapper for xclip")
      (description "This package provides wrapper for @code{xclip}.")
      (home-page #f)
      (license license:gpl3+))))
