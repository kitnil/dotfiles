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
                (display (output) error-port)
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

(define (juniper-configuration->vc config)
  (program-file
   (string-append "juniper-configuration-to-version-control-"
                  (juniper-configuration-host config))
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (guix build utils)
                      (ice-9 format)
                      (srfi srfi-34))
         (let* ((git #$(file-append git "/bin/git"))
                (git-commit
                 (lambda* (#:key git message output)
                   (with-directory-excursion output
                     (invoke git "add" ".")
                     (invoke git "commit" (string-append "--message=" message))
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
                             (loop)))))))
                (hostname #$(juniper-configuration-host config))
                (message (string-append hostname ": Update."))
                (directory
                 (string-append #$%ansible-state-directory "/" hostname "/config"))
                (file (string-append directory "/juniper.conf")))
           (setenv "PATH"
                   (string-append (getenv "PATH")
                                  ":" #$(file-append openssh "/bin")))
           (format #t "directory: ~s.~%" directory)
           (mkdir-p directory)
           (with-directory-excursion directory
             (call-with-output-file (string-append directory "/juniper.conf")
               (lambda (port)
                 (display #$(juniper-command (juniper-configuration-host config)
                                             '("cli" "show" "configuration"))
                          port)))
             (git-commit #:git git #:message message #:output directory)
             (call-with-output-file (string-append directory "/interfaces.txt")
               (lambda (port)
                 (display #$(juniper-command (juniper-configuration-host config)
                                             '("cli" "show" "interfaces" "detail"))
                          port)))
             (git-commit #:git git #:message message #:output directory)
             (call-with-output-file (string-append directory "/chassis-hardware.txt")
               (lambda (port)
                 (display #$(juniper-command (juniper-configuration-host config)
                                             '("cli" "show" "chassis" "hardware")) 
                          port)))
             (git-commit #:git git #:message message #:output directory)
             (call-with-output-file (string-append directory "/virtual-chassis.txt")
               (lambda (port)
                 (display #$(juniper-command (juniper-configuration-host config)
                                             '("cli" "show" "virtual-chassis"))
                          port)))
             (git-commit #:git git #:message message #:output directory)
             (call-with-output-file (string-append directory "/memory.txt")
               (lambda (port)
                 (display #$(juniper-command (juniper-configuration-host config)
                                             '("cli" "show" "system" "memory"))
                          port)))
             (git-commit #:git git #:message message #:output directory)
             (call-with-output-file (string-append directory "/processes.txt")
               (lambda (port)
                 (display #$(juniper-command (juniper-configuration-host config)
                                             '("cli" "show" "system" "processes"))
                          port)))
             (git-commit #:git git #:message message #:output directory)
             (call-with-output-file (string-append directory "/chassis-routing-engine.txt")
               (lambda (port)
                 (display #$(juniper-command (juniper-configuration-host config)
                                             '("cli" "show" "chassis" "routing-engine")) 
                          port)))
             (git-commit #:git git #:message message #:output directory)
             (call-with-output-file (string-append directory "/route.txt")
               (lambda (port)
                 (display #$(juniper-command (juniper-configuration-host config)
                                             '("cli" "show" "route" "0.0.0.0/0" "detail"))
                          port)))
             (git-commit #:git git #:message message #:output directory)
             (call-with-output-file (string-append directory "/route-summary.txt")
               (lambda (port)
                 (display #$(juniper-command (juniper-configuration-host config)
                                             '("cli" "show" "route" "summary"))
                          port)))
             (git-commit #:git git #:message message #:output directory)
             (call-with-output-file (string-append directory "/route-protocol-bgp.txt")
               (lambda (port)
                 (display #$(juniper-command (juniper-configuration-host config)
                                             '("cli" "show" "route" "protocol" "bgp"))
                          port)))
             (git-commit #:git git #:message message #:output directory)
             (call-with-output-file (string-append directory "/mac.txt")
               (lambda (port)
                 (display #$(juniper-command (juniper-configuration-host config)
                                             '("cli" "show" "ethernet-switching" "table" "brief"))
                          port)))
             (git-commit #:git git #:message message #:output directory)
             #$(juniper-configuration-post-hook config)
             (git-commit #:git git #:message message #:output directory)))))))

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
