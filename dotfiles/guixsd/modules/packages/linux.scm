(define-module (packages linux)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
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

(define linux-libre-deblob-scripts
  (@@ (gnu packages linux) linux-libre-deblob-scripts))

(define make-linux-libre-source
  (@@ (gnu packages linux) make-linux-libre-source))

(define %upstream-linux-source
  (@@ (gnu packages linux) %upstream-linux-source))

(define source-with-patches
  (@@ (gnu packages linux) source-with-patches))

(define %boot-logo-patch
  (@@ (gnu packages linux) %boot-logo-patch))

(define %linux-libre-arm-export-__sync_icache_dcache-patch
  (@@ (gnu packages linux) %linux-libre-arm-export-__sync_icache_dcache-patch))

(define make-linux-libre-headers*
  (@@ (gnu packages linux) make-linux-libre-headers*))

(define make-linux-libre*
  (@@ (gnu packages linux) make-linux-libre*))

(define kernel-config
  (@@ (gnu packages linux) kernel-config))

(define-public linux-libre-5.13-version "5.13.16")
(define-public linux-libre-5.13-gnu-revision "gnu1")
(define deblob-scripts-5.13
  (linux-libre-deblob-scripts
   linux-libre-5.13-version
   linux-libre-5.13-gnu-revision
   (base32 "0hj3w3vh1rj24xgl4v72mr6vaz1qzsnc5xzdfjga1zy84bw8lhkp")
   (base32 "1a0k9i8gnzkyvfr80f8xw2fnxfwddhz1pzicz9fh0y3jzzkzk45p")))
(define-public linux-libre-5.13-pristine-source
  (let ((version linux-libre-5.13-version)
        (hash (base32 "1ljigvcg4q6ckr8kna3q5iyjsy7x5mrf1ycqfy0ibbhn9hbqjna9")))
   (make-linux-libre-source version
                            (%upstream-linux-source version hash)
                            deblob-scripts-5.13)))

(define-public linux-libre-5.13-source
  (source-with-patches linux-libre-5.13-pristine-source
                       (list %boot-logo-patch
                             %linux-libre-arm-export-__sync_icache_dcache-patch)))

(define-public linux-libre-headers-5.13
  (make-linux-libre-headers* linux-libre-5.13-version
                             linux-libre-5.13-gnu-revision
                             linux-libre-5.13-source))

(define-public linux-libre-5.13
  (make-linux-libre* linux-libre-5.13-version
                     linux-libre-5.13-gnu-revision
                     linux-libre-5.13-source
                     '("x86_64-linux" "i686-linux" "armhf-linux" "aarch64-linux" "riscv64-linux")
                     #:configuration-file kernel-config))

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
