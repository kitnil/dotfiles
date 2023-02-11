(define-module (packages networking)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (guix build utils)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages python-xyz))

(define %home
  (and=> (getenv "HOME")
         (lambda (home)
           home)))

(define %source-dir
  (string-append %home "/.local/share/chezmoi"))

(define (git-output . args)
  "Execute 'git ARGS ...' command and return its output without trailing
newspace."
  (with-directory-excursion %source-dir
    (let* ((port   (apply open-pipe* OPEN_READ "git" args))
           (output (read-string port)))
      (close-pipe port)
      (string-trim-right output #\newline))))

(define (current-commit)
  (git-output "log" "-n" "1" "--pretty=format:%H"))

(define-public cisco
  (package
    (name "cisco")
    (version "1.0.0")
    (source (local-file %source-dir
                        #:recursive? #t
                        #:select? (git-predicate %source-dir)))
    (build-system python-build-system)
    (propagated-inputs
     (list python-pexpect))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "src/python-cisco"))))))
    (home-page "https://wugi.info")
    (synopsis "Run commands on Cisco hardware")
    (description "This package provides a Python program to run commands on
Cisco hardware.")
    (license license:gpl3+)))

(define-public cisco-interact
  (package
    (name "cisco-interact")
    (version "1.0.0")
    (source (local-file %source-dir
                        #:recursive? #t
                        #:select? (git-predicate %source-dir)))
    (build-system python-build-system)
    (propagated-inputs
     (list python-pexpect))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "src/python-cisco-interact"))))))
    (home-page "https://wugi.info")
    (synopsis "Connect to Cisco hardware and type a password")
    (description "This package provides a Python program to connect to Cisco
hardware and type a password automatically.")
    (license license:gpl3+)))

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
