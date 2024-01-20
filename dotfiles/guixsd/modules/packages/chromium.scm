(define-module (packages chromium)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages video)
  #:use-module (nongnu packages chromium)
  #:use-module (guix build-system cmake)
  #:use-module (nonguix build-system binary)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix svn-download)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module ((srfi srfi-1) #:hide (zip))
  #:use-module (srfi srfi-26))

(define-public chromium-embedded-framework-117
  (let ((git-revision "5053a95")
        (chromium-version "117.0.5938.150")
        (arch (match (or (%current-target-system) (%current-system))
                ("aarch64-linux" "linuxarm64")
                ("armhf-linux" "linuxarm")
                (_ "linux64"))))
    (package
      (inherit chromium-embedded-framework)
      (version "117.2.4")
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://cef-builds.spotifycdn.com/cef_binary_"
                      version
                      "+g" git-revision
                      "+chromium-" chromium-version
                      "_" arch "_minimal.tar.bz2"))
                (sha256
                 (base32
                  "0vzzwq1k6bv9d209yg3samvfnfwj7s58y9r3p3pd98wxa9iyzf4j")))))))
