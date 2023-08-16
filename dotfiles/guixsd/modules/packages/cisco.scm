(define-module (packages cisco)
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
