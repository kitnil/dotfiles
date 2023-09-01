(define-module (home services h3c)
  #:use-module (gnu home services)
  #:use-module (gnu home services mcron)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services mcron)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix store)
  #:use-module (home config)
  #:use-module (guile pass)
  #:export (h3c-configuration->vc-sw4-mr14.intr
            h3c-configuration->vc-sw4-mr13.intr
            h3c-configuration->vc-sw4-mr12.intr
            h3c-configuration->vc-sw4-mr11.intr))

(define (h3c-command host command)
  #~(begin
      (use-modules (ice-9 rdelim)
                   (ice-9 popen))
      (let* ((ssh-password-file
              (and=> (getenv "SSH_PASSWORD_FILE")
                     (lambda (file)
                       file)))
             (port (open-pipe* OPEN_READ
                               #$(file-append sshpass "/bin/sshpass")
                               (string-append "-p"
                                              (string-trim-right
                                               (with-input-from-file ssh-password-file
                                                 read-string)))
                               #$(file-append openssh "/bin/ssh")
                               #$host "--" #$@command))
             (output (read-string port)))
        (close-pipe port)
        output)))

(define (git-diff directory)
  "Return true if differences exist, false otherwise."
  #~(begin
      (use-modules (ice-9 rdelim)
                   (ice-9 popen))
      (let* ((port (open-pipe* OPEN_READ #$(file-append git "/bin/git")
                               "diff" directory))
             (output (read-string port)))
        (close-pipe port)
        (= (string-length output) 0))))

(define* (h3c-configuration->vc host)
  (program-file
   (string-append "h3c-configuration-to-version-control-" host)
   (with-imported-modules (append (source-module-closure '((guix utils)))
                                  '((guix build utils)))
     #~(begin
         (use-modules (guix build utils)
                      (guix utils)
                      (srfi srfi-34))
         (let* ((directory (string-append #$%ansible-state-directory "/" #$host "/config"))
                (file (string-append directory "/h3c.conf"))
                (git #$(file-append git "/bin/git")))
           (setenv "PATH"
                   (string-append (getenv "PATH")
                                  ":" #$(file-append openssh "/bin")))
           (mkdir-p directory)
           (call-with-output-file (string-append directory "/current-configuration.txt")
             (lambda (port)
               (display #$(h3c-command host '("show" "current-configuration")) port)))
           (call-with-output-file (string-append directory "/interface.txt")
             (lambda (port)
               (display #$(h3c-command host '("show" "interface")) port)))
           (call-with-output-file (string-append directory "/mac.txt")
             (lambda (port)
               (display #$(h3c-command host '("show" "mac-address")) port)))
           (with-directory-excursion #$%ansible-state-directory
             (when (not #$(git-diff host))
               (invoke git "add" #$host)
               (invoke git "commit" "--message=Update.")
               (let loop ()
                 (if (guard (c ((invoke-error? c)
                                (report-invoke-error c)
                                #f))
                       (invoke git "push" "origin"
                               (string-append "HEAD:" (or (getenv "GIT_BRANCH")
                                                          "master"))))
                     #t
                     (begin
                       (guard (c ((invoke-error? c)
                                  (report-invoke-error c)))
                         (invoke git "pull" "--rebase" "origin" "master"))
                       (loop))))))
           (and=> (getenv "SLEEP_SECONDS")
                  (lambda (seconds)
                    (sleep (string->number seconds)))))))))

(define h3c-configuration->vc-sw4-mr11.intr
  (h3c-configuration->vc "sw4-mr11.intr"))

(define h3c-configuration->vc-sw4-mr12.intr
  (h3c-configuration->vc "sw4-mr12.intr"))

(define h3c-configuration->vc-sw4-mr13.intr
  (h3c-configuration->vc "sw4-mr13.intr"))

(define h3c-configuration->vc-sw4-mr14.intr
  (h3c-configuration->vc "sw4-mr14.intr"))
