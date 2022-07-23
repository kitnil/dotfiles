(define-module (packages linux)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system linux-module)
  #:use-module (guix build-system trivial)
  #:use-module (ice-9 match)
  #:use-module (nonguix licenses)
  #:use-module (nongnu packages linux))

(define-public linux-5.13
  (corrupt-linux linux-libre-5.13 "5.13.16"
                 "1ljigvcg4q6ckr8kna3q5iyjsy7x5mrf1ycqfy0ibbhn9hbqjna9"))

(define-public linux-firmware
  (package
    (name "linux-firmware")
    (version "20210818")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://git.kernel.org/pub/scm/linux/kernel"
                                  "/git/firmware/linux-firmware.git/snapshot/"
                                  "linux-firmware-" version ".tar.gz"))
              (sha256
               (base32
                "0842k00kjh89497vvd7zy3j8d5xq180q2zkqmq0yivp2xkzvbwfc"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags (list (string-append "DESTDIR=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'validate-runpath))))
    (home-page
     "https://git.kernel.org/pub/scm/linux/kernel/git/firmware/linux-firmware.git")
    (synopsis "Nonfree firmware blobs for Linux")
    (description "Nonfree firmware blobs for enabling support for various
hardware in the Linux kernel.  This is a large package which may be overkill
if your hardware is supported by one of the smaller firmware packages.")
    (license
     (nonfree
      (string-append "https://git.kernel.org/pub/scm/linux/kernel/git/"
                     "firmware/linux-firmware.git/plain/WHENCE")))))
