(define-module (packages artwork)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:export (%local-artwork-repository))

(define %local-artwork-repository
  (let ((commit "549bf7e6408356f23f0f5d95275c4af6c08ac1a7"))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://gitlab.wugi.info:guix/guix-artwork.git")
            (commit commit)))
      (file-name (string-append "guix-artwork-" (string-take commit 7)
                                "-checkout"))
      (sha256
       (base32
	"0dkalnw6bvnm4x640b05xlfifhsjvk6889gwypmx0asqb0s6lnj6")))))

%local-artwork-repository
