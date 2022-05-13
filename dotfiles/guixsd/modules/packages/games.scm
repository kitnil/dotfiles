(define-module (packages games)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (guix build utils)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages python-xyz))

(define-public yuzu
  (package
    (name "yuzu")
    (version "20220511-f2087880e")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/yuzu-emu/yuzu-mainline/releases/download/mainline-0-1013/yuzu-"
         version ".AppImage"))
       (sha256
        (base32
         "1pgp1h6pdb5mqan1j1srx0pwig5cxa6xqckrjb7dcbbqpxd2j7f1"))))
    (build-system trivial-build-system)
    (inputs (list fuse gzip tar glibc patchelf zlib))
    (native-inputs `(("source" ,source)))
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (setenv "PATH"
                  (string-append
                   #$(this-package-input "gzip") "/bin"
                   ":" #$(this-package-input "tar") "/bin"
                   ":" #$(this-package-input "patchelf") "/bin"))
          (let* ((bin (string-append #$output "/bin"))
                 (yuzu (string-append bin "/yuzu")))
            (mkdir-p bin)
            (copy-file #$(this-package-native-input "source") yuzu)
            (chmod yuzu #o755)
            (invoke "patchelf"
                    "--set-interpreter"
                    (string-append #$(this-package-input "glibc")
                                   "/lib/ld-linux-x86-64.so.2")
                    "--set-rpath" (string-append
                                   #$(this-package-input "zlib") "/lib"
                                   ":" #$(this-package-input "fuse") "/lib")
                    yuzu)))))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:expat)))
