(define-module (packages linux)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages version-control)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system linux-module)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
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

(define-public drbd9
  (package
    (name "drbd9")
    (version "9.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pkg.linbit.com/downloads/drbd/9/drbd-"
                           version ".tar.gz"))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1iak07vpynimbyh4lhpf8xpn6vhgxnn3jmckm28r09m3a5adyrj1"))))
    (build-system linux-module-build-system)
    (native-inputs
     `(("source" ,source)
       ("gzip" ,gzip)
       ("tar" ,tar)))
    (inputs
     `(("curl" ,curl)))
    (arguments
     (list
      #:tests? #f                  ; no tests
      #:make-flags #~(let ((shell (search-input-file %build-inputs "/bin/bash")))
                       (list (string-append "CONFIG_SHELL=" shell)
                             (string-append "SHELL=" shell)))
      #:phases
      `(modify-phases %standard-phases
         (replace 'unpack
           (lambda args
             (setenv "PATH" (string-append
                             (assoc-ref %build-inputs "tar") "/bin"
                             ":" (assoc-ref %build-inputs "gzip") "/bin"
                             ":" (getenv "PATH")))
             (invoke "tar"
                     "-xf" (assoc-ref %build-inputs "source")
                     (string-append "drbd-" ,version)
                     "--strip-components=1")))
         (replace 'build
           (lambda args
             (for-each
              (lambda (module)
                (with-directory-excursion module
                  (apply (assoc-ref %standard-phases 'build) args)))
              '("drbd")))))))
    (home-page "https://gitlab.com/drbd9/drbd9")
    (synopsis "")
    (description "")
    (license #f)))
