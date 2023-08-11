(define-module (home services juniper)
  #:use-module (gnu home services)
  #:use-module (gnu home services mcron)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages version-control)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (home config)
  #:use-module (gnu services mcron)
  #:export (juniper-service-type

            juniper-configuration->vc-sr1-mr13-14.intr
            juniper-configuration->vc-sr1-dh507-508.intr
            juniper-configuration->vc-sw2-mr13.intr))

(define-record-type* <juniper-configuration>
  juniper-configuration make-juniper-configuration
  juniper-configuration?
  (host      juniper-configuration-host)    ;string
  (post-hook juniper-configuration-post-hook ;gexp
             (default #~(begin #t))))

(define (juniper-command host command)
  #~(begin
      (use-modules (ice-9 format)
                   (ice-9 rdelim)
                   (ice-9 popen))
      (let* ((ssh-password-file
              (and=> (getenv "SSH_PASSWORD_FILE")
                     (lambda (file)
                       file)))
             (run-command
              (lambda ()
                (let* ((port (open-pipe* OPEN_READ
                                         #$(file-append sshpass "/bin/sshpass")
                                         (string-append
                                          "-p"
                                          (string-trim-right
                                           (with-input-from-file ssh-password-file
                                             read-string)))
                                         #$(file-append openssh "/bin/ssh")
                                         #$host "--" #$@command))
                       (output-string (read-string port)))
                  (close-pipe port)
                  output-string)))
             (output (make-parameter (run-command)))
             (attempt (make-parameter 1))
             (error-port (current-error-port))
             (max-attempts 20))
        (format error-port "root@~a> ~a~%" #$host (string-join '#$command))
        (let loop ()
          (if (and (> (string-length (output)) 0))
              (begin
                (display (string-take (output) (* 72 4)) error-port)
                (newline error-port)
                (display "..." error-port)
                #t)
              (if (> max-attempts (attempt))
                  (begin
                    (format error-port "[~a/~a] attempt no output, running again...\n" (attempt) max-attempts)
                    (sleep 5)
                    (attempt (1+ (attempt)))
                    (output (run-command))
                    (loop))
                  (begin
                    (display "out of attempts, still no output, skipping...\n" error-port)
                    #t))))
        (newline error-port)
        (output))))

(define (juniper-configuration->file config)
  (program-file
   "juniper-show-configuration-program"
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (guix build utils))
         (let* ((directory
                 (string-append #$%ansible-state-directory "/" #$(juniper-configuration-host config) "/config"))
                (file (string-append directory "/juniper.conf")))
           (mkdir-p directory)
           (call-with-output-file (string-append directory "/juniper.conf")
             (lambda (port)
               (display #$(juniper-command (juniper-configuration-host config)
                                           '("cli" "show" "configuration"))
                        port)))
           (call-with-output-file (string-append directory "/interfaces.txt")
             (lambda (port)
               (display #$(juniper-command (juniper-configuration-host config)
                                           '("cli" "show" "interfaces" "detail"))
                        port)))
           (call-with-output-file (string-append directory "/chassis-hardware.txt")
             (lambda (port)
               (display #$(juniper-command (juniper-configuration-host config)
                                           '("cli" "show" "chassis" "hardware")) 
                        port)))
           (call-with-output-file (string-append directory "/virtual-chassis.txt")
             (lambda (port)
               (display #$(juniper-command (juniper-configuration-host config)
                                           '("cli" "show" "virtual-chassis"))
                        port)))
           (call-with-output-file (string-append directory "/memory.txt")
             (lambda (port)
               (display #$(juniper-command (juniper-configuration-host config)
                                           '("cli" "show" "system" "memory"))
                        port)))
           (call-with-output-file (string-append directory "/processes.txt")
             (lambda (port)
               (display #$(juniper-command (juniper-configuration-host config)
                                           '("cli" "show" "system" "processes"))
                        port)))
           (call-with-output-file (string-append directory "/chassis-routing-engine.txt")
             (lambda (port)
               (display #$(juniper-command (juniper-configuration-host config)
                                           '("cli" "show" "chassis" "routing-engine")) 
                        port)))
           (call-with-output-file (string-append directory "/route.txt")
             (lambda (port)
               (display #$(juniper-command (juniper-configuration-host config)
                                           '("cli" "show" "route" "0.0.0.0/0" "detail"))
                        port)))
           (call-with-output-file (string-append directory "/route-summary.txt")
             (lambda (port)
               (display #$(juniper-command (juniper-configuration-host config)
                                           '("cli" "show" "route" "summary"))
                        port)))
           (call-with-output-file (string-append directory "/route-protocol-bgp.txt")
             (lambda (port)
               (display #$(juniper-command (juniper-configuration-host config)
                                           '("cli" "show" "route" "protocol" "bgp"))
                        port)))
           (call-with-output-file (string-append directory "/mac.txt")
             (lambda (port)
               (display #$(juniper-command (juniper-configuration-host config)
                                           '("cli" "show" "ethernet-switching" "table" "brief"))
                        port)))
           #$(juniper-configuration-post-hook config))))))

(define (juniper-configuration->vc config)
  (program-file
   (string-append "juniper-configuration-to-version-control-"
                  (juniper-configuration-host config))
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (guix build utils))
         (let ((git #$(file-append git "/bin/git")))
           (setenv "PATH"
                   (string-append (getenv "PATH")
                                  ":" #$(file-append openssh "/bin")))
           (invoke #$(juniper-configuration->file config))
           (with-directory-excursion #$%ansible-state-directory
             (invoke git "add" #$(juniper-configuration-host config))
             (invoke git "commit" "--message=Update.")
             (invoke git "push" "origin"
                     (string-append "HEAD:" (or (getenv "GIT_BRANCH")
                                                "master")))))))))

(define (juniper-bgp-commands host)
  #~(begin
      (call-with-output-file (string-append directory "/bgp-summary.txt")
        (lambda (port)
          (display #$(juniper-command host
                                      '("cli" "show" "bgp" "summary"))
                   port)))
      (call-with-output-file (string-append directory "/chassis-pic.txt")
        (lambda (port)
          (display #$(juniper-command host
                                      '("cli" "show" "chassis" "pic" "pic-slot" "0" "fpc-slot" "0"))
                   port)
          (display #$(juniper-command host
                                      '("cli" "show" "chassis" "pic" "pic-slot" "0" "fpc-slot" "1"))
                   port)
          (display #$(juniper-command host
                                      '("cli" "show" "chassis" "pic" "pic-slot" "1" "fpc-slot" "0"))
                   port)
          (display #$(juniper-command host
                                      '("cli" "show" "chassis" "pic" "pic-slot" "1" "fpc-slot" "1"))
                   port)))
      (call-with-output-file (string-append directory "/bgp-neighbors.txt")
        (lambda (port)
          (display "group WEBA\n" port)
          (display #$(juniper-command host
                                      '("cli" "show" "bgp" "neighbor" "10.102.0.1"))
                   port)
          (display "group PROMETEY\n" port)
          (display #$(juniper-command host
                                      '("cli" "show" "bgp" "neighbor" "85.235.192.226"))
                   port)
          (display #$(juniper-command host
                                      '("cli" "show" "bgp" "neighbor" "85.235.198.236"))
                   port)
          (display "group DATAIX\n" port)
          (display #$(juniper-command host
                                      '("cli" "show" "bgp" "neighbor" "178.18.224.100"))
                   port)
          (display #$(juniper-command host
                                      '("cli" "show" "bgp" "neighbor" "178.18.227.100"))
                   port)
          (display "group SERVICE-PIPE\n" port)
          (display #$(juniper-command host
                                      '("cli" "show" "bgp" "neighbor" "10.70.0.21"))
                   port)
          (display #$(juniper-command host
                                      '("cli" "show" "bgp" "neighbor" "10.70.0.25"))
                   port)
          (display "group IBGP-OF\n" port)
          (display #$(juniper-command host
                                      '("cli" "show" "bgp" "neighbor" "10.10.3.100"))
                   port)
          (display "group IBGP-DH-BACKUP\n" port)
          (display #$(juniper-command host
                                      '("cli" "show" "bgp" "neighbor" "10.10.1.102"))
                   port)
          (display "group IBGP-DH\n" port)
          (display #$(juniper-command host
                                      '("cli" "show" "bgp" "neighbor" "10.10.0.3"))
                   port)
          (display "group IBGP-BORDER\n" port)
          (display #$(juniper-command host
                                      '("cli" "show" "bgp" "neighbor" "10.10.0.1"))
                   port)))

      (call-with-output-file (string-append directory "/route-advertising-protocol.txt")
        (lambda (port)
          (display "group PROMETEY\n" port)
          (display #$(juniper-command host
                                      '("cli" "show" "route" "advertising-protocol" "bgp" "85.235.192.226"))
                   port)
          (display #$(juniper-command host
                                      '("cli" "show" "route" "advertising-protocol" "bgp" "85.235.198.236"))
                   port)))))

(define juniper-configuration->vc-sr1-mr13-14.intr
  (juniper-configuration->vc
   (juniper-configuration
    (host "sr1-mr13-14.intr")
    (post-hook (juniper-bgp-commands host)))))

(define juniper-configuration->vc-sr1-dh507-508.intr
  (juniper-configuration->vc
   (juniper-configuration
    (host "sr1-dh507-508.intr")
    (post-hook (juniper-bgp-commands host)))))

(define juniper-configuration->vc-sw2-mr13.intr
  (juniper-configuration->vc
   (juniper-configuration
    (host "sw2-mr13.intr"))))

(define (juniper-mcron-jobs config)
  (list
   #~(job
      '(next-hour '(21))
      #$(run-with-store (open-connection)
          (lower-object juniper-configuration->vc-sr1-mr13-14.intr)))
   #~(job
      '(next-hour '(22))
      #$(run-with-store (open-connection)
          (lower-object juniper-configuration->vc-sr1-dh507-508.intr)))
   #~(job
      '(next-hour '(23))
      #$(run-with-store (open-connection)
          (lower-object juniper-configuration->vc-sw2-mr13.intr)))))

(define juniper-service-type
  (service-type
   (name 'juniper)
   (extensions
    (list (service-extension mcron-service-type
                             juniper-mcron-jobs)))
   (description
    "Periodically run Juniper configuration dump.")
   (default-value '())))
