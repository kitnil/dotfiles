(use-modules ((guix licenses) #:prefix license:)
             (gnu packages base)
             (gnu packages bash)
             (guix build utils)
             (guix build utils)
             (guix build-system trivial)
             (guix gexp)
             (guix gexp)
             (guix git-download)
             (guix git-download)
             (guix packages)
             (guix packages)
             (ice-9 popen)
             (ice-9 rdelim))

(define %source-dir "/home/oleg/.local/share/chezmoi")

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

(let ((commit (current-commit)))
  (package
    (name "dotfiles")
    (version (git-version "2.0.0" "1" (string-take commit 7)))
    (source (local-file %source-dir
                        #:recursive? #t
                        #:select? (git-predicate %source-dir)))
    (build-system trivial-build-system)
    (inputs
     `(("bash" ,bash)
       ("make" ,gnu-make)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((bin (string-append %output "/bin"))
               (dotfiles (string-append %output "/share/dotfiles"))
               (executable (string-append %output "/bin/dotfiles")))
           (copy-recursively (assoc-ref %build-inputs "source") ".")
           (substitute* "bin/executable_dotfiles"
             (("@GUIX@") dotfiles)
             (("/bin/sh")
              (string-append (assoc-ref %build-inputs "bash") "/bin/bash")))

           (mkdir-p dotfiles)
           (copy-recursively "." dotfiles)

           (mkdir-p bin)
           (copy-file "bin/executable_dotfiles" executable)
           (chmod executable #o555))
         #t)))
    (home-page "https://github.com/kitnil/dotfiles")
    (license license:gpl3+)
    (synopsis "WiGust dotfiles")
    (description "This package provides wigust dotfiles which could
be installed with chezmoi.")))
