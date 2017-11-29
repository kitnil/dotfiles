(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (guix build utils)
             (guix packages)
             (guix git-download)
             (guix gexp)
             (guix build-system trivial))


;;;
;;; This Git repository
;;;

(define %current-directory
  (dirname (current-filename)))

(define (git-output . args)
  "Execute 'git ARGS ...' command and return its output without trailing
newspace."
  (with-directory-excursion %current-directory
    (let* ((port   (apply open-pipe* OPEN_READ "git" args))
           (output (read-string port)))
      (close-port port)
      (string-trim-right output #\newline))))

(define %source-dir (git-output "-C" %current-directory
                                "rev-parse" "--show-toplevel"))

(define (current-commit)
  (git-output "log" "-n" "1" "--pretty=format:%H"))

(define fiore-dotfiles
  (package
    (name "fiore-dotfiles")
    (version (string-append "0.0.1"
                            "-" (string-take (current-commit) 7)))
    (source (local-file %source-dir
                        #:recursive? #t
                        #:select? (git-predicate %source-dir)))
    (build-system trivial-build-system)
    (description "fiore")
    (synopsis "fiore")
    (home-page "http://www.magnolia.local")
    (license #f)))

fiore-dotfiles
