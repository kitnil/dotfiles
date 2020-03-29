(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (guix build utils)
             (guix gexp)
             (guix git-download)
             (guix packages)
             (guix build-system trivial)
             ((guix licenses) #:prefix license:))

(define %source-dir (dirname (dirname (dirname (current-filename)))))

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

(define dotfiles
  (let ((commit (current-commit)))
    (package
      (name "dotfiles")
      (version (git-version "0.0.1" "1" commit))
      (source (local-file %source-dir
                          #:recursive? #t
                          #:select? (git-predicate %source-dir)))
      (build-system trivial-build-system)
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
               (("@GUIX@") dotfiles))

             (mkdir-p dotfiles)
             (copy-recursively "." dotfiles)

             (mkdir-p bin)
             (copy-file "bin/executable_dotfiles" executable)
             (chmod executable #o555))
           #t)))
      (home-page "https://gitlab.com/wigust/dotfiles/")
      (license license:gpl3+)
      (synopsis "")
      (description ""))))

dotfiles

;;; guix.scm ends here
