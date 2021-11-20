(define-module (guile gpg)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen)
  #:export (gpg->file))

(define (gpg->file gpg file)
  (call-with-output-file file
    (lambda (file-port)
      (let* ((port (open-pipe* OPEN_READ
                               "gpg" "--quiet" "--for-your-eyes-only" "--no-tty"
                               "--decrypt" gpg))
             (output (read-string port)))
        (close-port port)
        (display (string-trim-right output #\newline) file-port)
        (newline file-port)))))
