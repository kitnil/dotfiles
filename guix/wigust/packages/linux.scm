(define-module (wigust packages linux)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system linux-module)
  #:use-module ((guix licenses) #:prefix license:))

(define-public vendor-reset-linux-module
  (let ((revision "1")
        (commit "225a49a40941e350899e456366265cf82b87ad25"))
    (package
      (name "vendor-reset-linux-module")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gnif/vendor-reset")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "071zd8slra0iqsvzqpp6lcvg5dql5hkn161gh9aq34wix7pwzbn5"))))
      (build-system linux-module-build-system)
      (arguments
       `(#:tests? #f                      ; no tests
                  ))
      (home-page "https://github.com/gnif/vendor-reset")
      (synopsis "Linux kernel module to perform GPU reset method calls")
      (description "")
      (license license:gpl3+))))
