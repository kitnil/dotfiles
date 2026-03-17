(define-module (wugi packages wayland)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages pkg-config)
  #:use-module (wugi packages rust-crates))

(define-public poe-scroll-click
  (let ((commit "366c63ac6826a82cfcfaef2bd4229834d75b39a9")
        (revision "1"))
    (package
      (name "poe-scroll-click")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/jchantrell/poe-scroll-click")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "07xgpm4cnhxswq57yajhbyqr02r6vm67w9lvwbx0mg1m6c7zb6nf"))))
      (build-system cargo-build-system)
      (arguments
       (list #:install-source? #f
             #:tests? #f                  ;Requires running display server.
             #:cargo-install-paths ''(".")))
      (native-inputs (list pkg-config))
      (inputs (cargo-inputs 'poe-scroll-click
                            #:module '(wugi packages rust-crates)))
      (home-page "https://github.com/jchantrell/poe-scroll-click")
      (synopsis "Scroll to click inputs for Path of Exile")
      (description "Scroll to click inputs for Path of Exile.
Emulates X-Mouse Button Control setups for Windows.  Remaps CTRL+scroll to
left click for Path of Exile 1 & 2 on Linux/Wayland.  Created to emulate
X-Mouse Button Control scroll-to-click setups for looting and inventory
management. It runs for all windows regardless of focus when either game is
running currently. That might change in the future if I work out a good way to
do it seamlessly for all Wayland WMs.")
      (license license:mpl2.0))))
