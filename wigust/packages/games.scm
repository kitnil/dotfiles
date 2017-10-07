(define-module (wigust packages games)
  #:use-module (ice-9 match)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (guix build-system trivial))

(define-public stb
  (let ((commit "9d9f75eb682dd98b34de08bb5c489c6c561c9fa6")
        (revision "1"))
    (package
      (name "stb")
      (version (string-append "0.0.1-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "git://github.com/nothings/stb.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "0q84bl6ai2mrcywrynvqvvlr6dpyafx33j3xaz6n38z5gi8lcmzx"))))
      (build-system trivial-build-system)
      (inputs `(("source" ,source)))
      (arguments
       `(#:modules
         ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (for-each (lambda (file)
                       (install-file file (string-append %output
                                                         "/include/stb")))
                     (find-files (assoc-ref %build-inputs "source")
                                 "\\.h$")))))
      (home-page "https://github.com/nothings/stb")
      (synopsis "stb single-file public domain libraries for C/C++")
      (description "stb single-file public domain libraries for C/C++")
      (license license:expat))))
