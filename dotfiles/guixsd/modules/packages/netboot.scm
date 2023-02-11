(define-module (packages netboot)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (guix build utils)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression))

(define-public netboot-xyz-efi
  (package
    (name "netboot-xyz-efi")
    (version "2.0.66")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/netbootxyz/netboot.xyz/releases/download/"
             version "/netboot.xyz.efi"))
       (sha256
        (base32
         "0m30gil2kpxw5vrdfi0vymfd7x1ybrihrqq7dkkxh168y36ngqxw"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `((,(assoc-ref %build-inputs "source")
          "/share/netboot-xyz/netboot-xyz.efi"))
       #:phases (modify-phases %standard-phases (delete 'unpack))))
    (home-page "https://netboot.xyz/")
    (synopsis
     "Network-based bootable operating system installer based on iPXE")
    (description "This package provides a network-based bootable operating
system installer based on iPXE.")
    (license license:asl2.0)))
