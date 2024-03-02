(define-module (packages linux)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
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
  #:use-module (nonguix licenses))

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

(define %bpf-extra-linux-options
  (@@ (gnu packages linux) %bpf-extra-linux-options))

(define %default-extra-linux-options
  (@@ (gnu packages linux) %default-extra-linux-options))

(define-public linux-libre-5.13-with-bpf
  (let ((base-linux-libre
         (make-linux-libre*
          linux-libre-5.13-version
          linux-libre-5.13-gnu-revision
          linux-libre-5.13-source
          '("x86_64-linux" "i686-linux" "armhf-linux"
            "aarch64-linux" "powerpc64le-linux" "riscv64-linux")
          #:extra-version "bpf"
          #:configuration-file kernel-config
          #:extra-options
          (append %bpf-extra-linux-options
                  %default-extra-linux-options))))
    (package
      (inherit base-linux-libre)
      (inputs (modify-inputs (package-inputs base-linux-libre)
                             (prepend cpio)))
      (synopsis "Linux-libre with BPF support")
      (description "This package provides GNU Linux-Libre with support
for @acronym{BPF, the Berkeley Packet Filter}."))))

(define (linux-urls version)
  "Return a list of URLS for Linux VERSION."
  (list (string-append "https://www.kernel.org/pub/linux/kernel/v"
                       (version-major version) ".x/linux-" version ".tar.xz")))

(define* (corrupt-linux freedo version hash #:key (name "linux"))
  (package
    (inherit freedo)
    (name name)
    (version version)
    (source (origin
              (method url-fetch)
              (uri (linux-urls version))
              (sha256 (base32 hash))))
    (home-page "https://www.kernel.org/")
    (synopsis "Linux kernel with nonfree binary blobs included")
    (description
     "The unmodified Linux kernel, including nonfree blobs, for running Guix
System on hardware which requires nonfree software to function.")))

(define-public linux-5.13
  (corrupt-linux linux-libre-5.13 "5.13.16"
                 "1ljigvcg4q6ckr8kna3q5iyjsy7x5mrf1ycqfy0ibbhn9hbqjna9"))

(define-public linux-5.13-with-bpf
  (corrupt-linux linux-libre-5.13-with-bpf "5.13.16"
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

(define-public drbd-module
  (package
    (name "drbd-module")
    (version "9.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pkg.linbit.com/downloads/drbd/9/drbd-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1iak07vpynimbyh4lhpf8xpn6vhgxnn3jmckm28r09m3a5adyrj1"))))
    (build-system linux-module-build-system)
    (inputs
     `(("bash" ,bash)))
    (arguments
     (list
      #:tests? #f ;there are none.
      #:source-directory "drbd"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-kbuild
            (lambda _
              ;; Patch files to refer to executables in the store or $PATH.
              (substitute* "drbd/Kbuild"
                (("/bin/bash") (which "bash")))
              #t))
          (add-after 'install 'rename-kernel-module
            (lambda _
              (use-modules (guix build utils)
                           (ice-9 string-fun))
              ;; Rename drbd to drbd9 because of modprobe loads drbd module
              ;; provided by the kernel instead of the current package.
              (for-each (lambda (file)
                          (rename-file file
                                       (string-replace-substring (string-replace-substring file
                                                                                           "drbd.ko"
                                                                                           "drbd9.ko")
                                                                 "drbd_transport_tcp.ko"
                                                                 "drbd9_transport_tcp.ko")))
                        (find-files #$output))
              #t)))))
    (home-page "https://github.com/linux-thinkpad/tp_smapi")
    (synopsis
     "Linux Kernel module exposing features of ThinkPad hardware")
    (description
     "This package provides a Linux Kernel module that controls
battery charging of specific ThinkPad laptops.  It also includes an improved
version of the HDAPS driver.  The underlying hardware interfaces are
@acronym{SMAPI, System Management Application Program Interface} and direct
access to the embedded controller.")
    (license #f)))
