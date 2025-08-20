(define-module (wugi packages linux-modules)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages patchutils)
  #:use-module (gnu packages python)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system linux-module)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26))

(define-public drbd-module
  (package
    (name "drbd-module")
    (version "9.2.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pkg.linbit.com/downloads/drbd/9/drbd-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1pvpj8ir2y1dysld0pmmgbdbhkwmslmpws5wsynp41pjyic2gjn7"))))
    (build-system linux-module-build-system)
    (native-inputs (list bash
                         coreutils ;md5sum
                         flex
                         coccinelle
                         python
                         sed
                         tar
                         gzip))
    (arguments
     (list
      #:modules `((guix build linux-module-build-system)
                  ((guix build gnu-build-system) #:prefix gnu:)
                  (guix build utils))
      #:tests? #f ;there are none.
      #:make-flags
      #~(list "SPAAS=false"
              (string-append "KDIR=" (assoc-ref %build-inputs "linux-module-builder")
                             "/lib/modules/build")
              (string-append "MODULE_DIR=" (string-append #$output "/lib/modules"))
              (string-append "INSTALL_PATH=" #$output)
              (string-append "INSTALL_MOD_PATH=" #$output)
              "INSTALL_MOD_STRIP=1")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-kbuild
            (lambda _
              ;; Patch files to refer to executables in the store or $PATH.
              (substitute* "drbd/Kbuild"
                (("/bin/bash") (which "bash")))
              (substitute* "drbd/Makefile.spatch"
                (("/bin/bash") (which "bash"))
                (("md5sum")
                 (string-append #$(this-package-native-input "coreutils")
                                "/bin/md5sum")))
              #t))
          (replace 'build (assoc-ref gnu:%standard-phases 'build))
          (add-after 'install 'gnu:install
            (assoc-ref gnu:%standard-phases 'install)))))
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
