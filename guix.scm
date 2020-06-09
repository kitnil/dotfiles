(use-modules
 (ice-9 popen)
 (ice-9 rdelim)
 (guix build utils)
 (guix gexp)
 (guix git-download)
 (guix packages)
 (wigust packages dotfiles))

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
    (inherit dotfiles)
    (version (string-append (package-version dotfiles)
                            "-" (string-take commit 7)))
    (source (local-file %source-dir
                        #:recursive? #t
                        #:select? (git-predicate %source-dir)))))
