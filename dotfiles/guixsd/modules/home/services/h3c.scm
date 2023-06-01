(define-module (home services h3c)
  #:use-module (gnu home services)
  #:use-module (gnu home services mcron)
  #:use-module (gnu packages ssh)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (home config)
  #:use-module (guile pass)
  #:export (h3c-configuration->vc-sw4-mr14.intr
            h3c-configuration->vc-sw4-mr13.intr
            h3c-configuration->vc-sw4-mr12.intr
            h3c-configuration->vc-sw4-mr11.intr

            h3c-service-type))

(define (h3c-command host command)
  #~(begin
      (use-modules (ice-9 rdelim)
                   (ice-9 popen))
      (let* ((port (open-pipe* OPEN_READ
                               #$(file-append sshpass "/bin/sshpass")
                               (string-append "-p"
                                              #$(pass "show" "majordomo/private/ssh/router"))
                               #$(file-append openssh "/bin/ssh")
                               #$host "--" #$@command))
             (output (read-string port)))
        (close-port port)
        output)))

(define* (h3c-configuration->vc host #:optional ssh?)
  (program-file
   (string-append "h3c-configuration-to-version-control-" host)
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (guix build utils)
                      (guix utils))
         (let* ((directory (string-append #$%ansible-state-directory "/" #$host "/config"))
                (file (string-append directory "/h3c.conf")))
           (mkdir-p directory)
           (call-with-output-file (string-append directory "/current-configuration.txt")
             (lambda (port)
               (display #$(h3c-command host '("show" "current-configuration")) port)))
           (call-with-output-file (string-append directory "/interface.txt")
             (lambda (port)
               (display #$(h3c-command host '("show" "interface")) port)))
           (call-with-output-file (string-append directory "/mac.txt")
             (lambda (port)
               (display #$(h3c-command host '("show" "mac-address")) port))))
         (with-directory-excursion #$%ansible-state-directory
           (invoke "git" "add" #$host)
           (invoke "git" "commit" "--message=Update."))))))

(define h3c-configuration->vc-sw4-mr11.intr
  (h3c-configuration->vc "sw4-mr11.intr"))

(define h3c-configuration->vc-sw4-mr12.intr
  (h3c-configuration->vc "sw4-mr12.intr"))

(define h3c-configuration->vc-sw4-mr13.intr
  (h3c-configuration->vc "sw4-mr13.intr"))

(define h3c-configuration->vc-sw4-mr14.intr
  (h3c-configuration->vc "sw4-mr14.intr"))

(define (h3c-mcron-jobs config)
  (list
   #~(job
      '(next-hour '(20))
      #$h3c-configuration->vc-sw4-mr14.intr)
   #~(job
      '(next-hour '(21))
      #$h3c-configuration->vc-sw4-mr13.intr)
   #~(job
      '(next-hour '(22))
      #$h3c-configuration->vc-sw4-mr12.intr)
   #~(job
      '(next-hour '(23))
      #$h3c-configuration->vc-sw4-mr11.intr)))

(define h3c-service-type
  (service-type
   (name 'h3c)
   (extensions
    (list (service-extension home-mcron-service-type
                             h3c-mcron-jobs)))
   (description
    "Periodically run h3c configuration dump.")
   (default-value '())))
