(define-module (packages networking)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (guix build utils)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages python-xyz)
  #:use-module (home services cisco)
  #:use-module (home services h3c)
  #:use-module (home services juniper)
  #:use-module (utils package))

(define-public plumber
  (package
    (name "plumber")
    (version "1.10.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/batchcorp/plumber/releases/download/v"
             version "/plumber-linux"))
       (sha256
        (base32
         "0dn2mp1q5j74ivymp48j0sicfrkiwq5rm9izi377znya6dvlki4j"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (mkdir-p (string-append %output "/bin"))
         (copy-file (assoc-ref %build-inputs "source")
                    (string-append %output "/bin/plumber"))
         (chmod (string-append %output "/bin/plumber")
                #o555))))
    (home-page "https://github.com/batchcorp/plumber/")
    (synopsis "AMQP command line client")
    (description "This package provides a a CLI devtool for inspecting,
piping, massaging and redirecting data in message systems like Kafka,
RabbitMQ, GCP PubSub and many more.

The tool enables you to:
@itemize
@item Safely view the contents of your data streams
@item Write plain or encoded data to any system
@item Route data from one place to another
@item Decode protobuf/avro/thrift/JSON data in real-time
@item Relay data to the Batch platform
@item Ship change data capture events to Batch platform
@item Replay events into a message system on your local network
@item And many other features (for a full list: plumber -h)
@end itemize

It's like curl for messaging systems.")
    (license license:expat)))

(define-public state-to-vc-sw4-mr11
  (package-from-program-file h3c-configuration->vc-sw4-mr11.intr))

(define-public state-to-vc-sw4-mr12
  (package-from-program-file h3c-configuration->vc-sw4-mr12.intr))

(define-public state-to-vc-sw4-mr13
  (package-from-program-file h3c-configuration->vc-sw4-mr13.intr))

(define-public state-to-vc-sw4-mr14
  (package-from-program-file h3c-configuration->vc-sw4-mr14.intr))

(define-public state-to-vc-sw1-dh507
  (package-from-program-file cisco-configuration->vc-sw1-dh507.intr))

(define-public state-to-vc-sw2-dh507
  (package-from-program-file cisco-configuration->vc-sw2-dh507.intr))

(define-public state-to-vc-sw1-dh508
  (package-from-program-file cisco-configuration->vc-sw1-dh508.intr))

(define-public state-to-vc-sw2-dh508
  (package-from-program-file cisco-configuration->vc-sw2-dh508.intr))

(define-public state-to-vc-sw1-mr11
  (package-from-program-file cisco-configuration->vc-sw1-mr11.intr))

(define-public state-to-vc-sw1-mr12
  (package-from-program-file cisco-configuration->vc-sw1-mr12.intr))

(define-public state-to-vc-sw2-mr12
  (package-from-program-file cisco-configuration->vc-sw2-mr12.intr))

(define-public state-to-vc-sw3-mr13
  (package-from-program-file cisco-configuration->vc-sw3-mr13.intr))

(define-public state-to-vc-sw1-mr14
  (package-from-program-file cisco-configuration->vc-sw1-mr14.intr))

(define-public state-to-vc-sw2-mr14
  (package-from-program-file cisco-configuration->vc-sw2-mr14.intr))

(define-public state-to-vc-sr1-mr13-14
  (package-from-program-file juniper-configuration->vc-sr1-mr13-14.intr))

(define-public state-to-vc-sr1-dh507-508
  (package-from-program-file juniper-configuration->vc-sr1-dh507-508.intr))

(define-public state-to-vc-sw2-mr13
  (package-from-program-file juniper-configuration->vc-sw2-mr13.intr))

(define-public byedpi
  (package
    (name "byedpi")
    (version "0.12")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hufrea/byedpi")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wchx2mbkmmiv84qk4plax70nqw7j58mlami6wnwdcjkdk5h6bid"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ;no tests
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda _
              (setenv "CC" "gcc")
              #t))
          (replace 'install
            (lambda _
              (mkdir-p (string-append #$output "/bin"))
              (copy-file "ciadpi" (string-append #$output "/bin/ciadpi"))
              (mkdir-p (string-append #$output "/share/doc/"
                                      #$name "-" #$version))
              (copy-file "readme.txt"
                         (string-append #$output "/share/doc/"
                                        #$name "-" #$version
                                        "/readme.txt")))))))
    (home-page "https://github.com/hufrea/byedpi")
    (synopsis "Bypass DPI")
    (description "Implementation of some DPI bypass methods.  The program is a
 local SOCKS proxy server.")
    (license license:expat)))
