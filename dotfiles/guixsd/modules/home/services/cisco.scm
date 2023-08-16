(define-module (home services cisco)
  #:use-module (gnu home services)
  #:use-module (gnu home services mcron)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix store)
  #:use-module (home config)
  #:use-module (gnu packages ssh)
  #:use-module (packages ssh)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services mcron)
  #:use-module (packages cisco)
  #:use-module (guile pass)
  #:export (cisco-service-type

            cisco-configuration->vc-sw1-dh507.intr
            cisco-configuration->vc-sw2-dh507.intr
            cisco-configuration->vc-sw1-dh508.intr
            cisco-configuration->vc-sw2-dh508.intr
            cisco-configuration->vc-sw1-mr11.intr
            cisco-configuration->vc-sw1-mr12.intr
            cisco-configuration->vc-sw2-mr12.intr
            cisco-configuration->vc-sw3-mr13.intr
            cisco-configuration->vc-sw1-mr14.intr
            cisco-configuration->vc-sw2-mr14.intr))

(define* (cisco-command host command #:optional ssh?)
  #~(begin
      (use-modules (ice-9 rdelim)
                   (ice-9 popen))
      (let ((ssh-password-file
             (and=> (getenv "SSH_PASSWORD_FILE")
                    (lambda (file)
                      file)))
            (telnet-password-file
             (and=> (getenv "TELNET_PASSWORD_FILE")
                    (lambda (file)
                      file))))
        #$(if ssh?
              #~(let* ((port (open-pipe* OPEN_READ
                                         #$(file-append sshpass "/bin/sshpass")
                                         (string-append
                                          "-p"
                                          (string-trim-right
                                           (with-input-from-file ssh-password-file
                                             read-string)))
                                         #$(file-append openssh-8.6p1 "/bin/ssh") "--"
                                         #$host #$@command))
                       (output (read-string port)))
                  (close-pipe port)
                  output)
              #~(with-environment-variables
                 `(("TELNET_PASSWORD"
                    ,(string-trim-right
                      (with-input-from-file telnet-password-file
                        read-string)))
                   ("ENABLE_PASSWORD"
                    ,(string-trim-right
                      (with-input-from-file ssh-password-file
                        read-string))))
                 (let* ((port (open-pipe* OPEN_READ
                                          #$(file-append cisco "/bin/cisco")
                                          #$host #$@command))
                        (output (read-string port)))
                   (close-pipe port)
                   output))))))

(define* (cisco-configuration->vc host #:optional ssh?)
  (program-file
   (string-append "cisco-configuration-to-version-control-" host)
   (with-imported-modules (append (source-module-closure '((guix utils)))
                                  '((guix build utils)))
     #~(begin
         (use-modules (guix build utils)
                      (guix utils))
         (let* ((directory (string-append #$%ansible-state-directory "/" #$host "/config"))
                (file (string-append directory "/cisco.conf"))
                (git #$(file-append git "/bin/git")))
           (setenv "PATH"
                   (string-append #$(file-append inetutils "/bin")
                                  ":" #$(file-append openssh-8.6p1 "/bin")
                                  ":" (getenv "PATH")))
           (mkdir-p directory)
           (call-with-output-file (string-append directory "/cisco.conf")
             (lambda (port)
               (display #$(cisco-command host '("show" "running-config") ssh?) port)))
           (call-with-output-file (string-append directory "/interfaces.txt")
             (lambda (port)
               (display #$(cisco-command host '("show" "vlan") ssh?) port)))
           (call-with-output-file (string-append directory "/vlan.txt")
             (lambda (port)
               (display #$(cisco-command host '("show" "interfaces") ssh?) port)))
           (call-with-output-file (string-append directory "/mac.txt")
             (lambda (port)
               (display #$(cisco-command host '("show" "mac-address-table") ssh?) port)))
           (with-directory-excursion #$%ansible-state-directory
             (invoke git "add" #$host)
             (invoke git "commit" "--message=Update.")
             (invoke git "push" "origin"
                     (string-append "HEAD:" (or (getenv "GIT_BRANCH")
                                                "master")))))))))

(define cisco-configuration->vc-sw1-dh507.intr
  (cisco-configuration->vc "sw1-dh507.intr"))

(define cisco-configuration->vc-sw2-dh507.intr
  (cisco-configuration->vc "sw2-dh507.intr" #t))

(define cisco-configuration->vc-sw1-dh508.intr
  (cisco-configuration->vc "sw1-dh508.intr"))

(define cisco-configuration->vc-sw2-dh508.intr
  (cisco-configuration->vc "sw2-dh508.intr" #t))

(define cisco-configuration->vc-sw1-mr11.intr
  (cisco-configuration->vc "sw1-mr11.intr"))

(define cisco-configuration->vc-sw1-mr12.intr
  (cisco-configuration->vc "sw1-mr12.intr"))

(define cisco-configuration->vc-sw2-mr12.intr
  (cisco-configuration->vc "sw2-mr12.intr"))

(define cisco-configuration->vc-sw3-mr13.intr
  (cisco-configuration->vc "sw3-mr13.intr" #t))

(define cisco-configuration->vc-sw1-mr14.intr
  (cisco-configuration->vc "sw1-mr14.intr"))

(define cisco-configuration->vc-sw2-mr14.intr
  (cisco-configuration->vc "sw2-mr14.intr" #t))

(define (cisco-mcron-jobs config)
  (list
   #~(job '(next-hour '(13))
          #$cisco-configuration->vc-sw1-dh507.intr)
   #~(job '(next-hour '(14))
          #$cisco-configuration->vc-sw2-dh507.intr)
   #~(job '(next-hour '(15))
          #$cisco-configuration->vc-sw1-dh508.intr)
   #~(job '(next-hour '(16))
          #$cisco-configuration->vc-sw2-dh508.intr)
   #~(job '(next-hour '(17))
          #$cisco-configuration->vc-sw1-mr11.intr)
   #~(job '(next-hour '(18))
          #$cisco-configuration->vc-sw1-mr12.intr)
   #~(job '(next-hour '(19))
          #$cisco-configuration->vc-sw2-mr12.intr)
   #~(job '(next-hour '(20))
          #$cisco-configuration->vc-sw3-mr13.intr)
   #~(job '(next-hour '(21))
          #$cisco-configuration->vc-sw1-mr14.intr)
   #~(job '(next-hour '(22))
          #$cisco-configuration->vc-sw2-mr14.intr)))

(define cisco-service-type
  (service-type
   (name 'cisco)
   (extensions
    (list (service-extension mcron-service-type
                             cisco-mcron-jobs)))
   (description
    "Periodically run Cisco configuration dump.")
   (default-value '())))
