(define-module (home services juniper)
  #:use-module (gnu home services)
  #:use-module (gnu home services mcron)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (home config)
  #:export (juniper-service-type

            juniper-configuration->vc-sr1-mr13-14.intr
            juniper-configuration->vc-sr1-dh507-508.intr))

(define (juniper-command host command)
  #~(begin
      (use-modules (ice-9 format)
                   (ice-9 rdelim)
                   (ice-9 popen))
      (let* ((run-command
              (lambda ()
                (let* ((port (open-pipe* OPEN_READ #$%connect-program #$host #$@command))
                       (output-string (read-string port)))
                  (close-port port)
                  output-string)))
             (output (make-parameter (run-command)))
             (attempt (make-parameter 1))
             (error-port (current-error-port)))
        (format error-port "root@~a> ~a~%" #$host (string-join '#$command))
        (let loop ()
          (if (and (> (string-length (output)) 0)
                   (< (attempt) 11))
              (begin
                (display (string-take (output) (* 72 4)) error-port)
                (newline error-port)
                (display "..." error-port)
                #t)
              (begin
                (format error-port "~a attempt no output, running again...\n" (attempt))
                (sleep 5)
                (attempt (1+ (attempt)))
                (output (run-command))
                (loop))))
        (newline error-port)
        (output))))

(define (juniper-configuration->file host)
  (program-file
   "juniper-show-configuration-program"
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (guix build utils))
         (let* ((directory
                 (string-append #$%ansible-state-directory "/" #$host "/config"))
                (file (string-append directory "/juniper.conf")))
           (mkdir-p directory)
           (call-with-output-file (string-append directory "/juniper.conf")
             (lambda (port)
               (display #$(juniper-command host '("cli" "show" "configuration")) port)))
           (call-with-output-file (string-append directory "/dhcp.txt")
             (lambda (port)
               (display #$(juniper-command host '("cli" "show" "system" "services" "dhcp" "binding")) port)))
           (call-with-output-file (string-append directory "/interfaces.txt")
             (lambda (port)
               (display #$(juniper-command host '("cli" "show" "interfaces" "detail")) port)))
           (call-with-output-file (string-append directory "/bgp-summary.txt")
             (lambda (port)
               (display #$(juniper-command host '("cli" "show" "bgp" "summary")) port)))
           (call-with-output-file (string-append directory "/chassis-hardware.txt")
             (lambda (port)
               (display #$(juniper-command host '("cli" "show" "chassis" "hardware")) port)))
           (call-with-output-file (string-append directory "/chassis-routing-engine.txt")
             (lambda (port)
               (display #$(juniper-command host '("cli" "show" "chassis" "routing-engine")) port)))
           (call-with-output-file (string-append directory "/chassis-pic.txt")
             (lambda (port)
               (display #$(juniper-command host '("cli" "show" "chassis" "pic" "pic-slot" "0" "fpc-slot" "0")) port)
               (display #$(juniper-command host '("cli" "show" "chassis" "pic" "pic-slot" "0" "fpc-slot" "1")) port)
               (display #$(juniper-command host '("cli" "show" "chassis" "pic" "pic-slot" "1" "fpc-slot" "0")) port)
               (display #$(juniper-command host '("cli" "show" "chassis" "pic" "pic-slot" "1" "fpc-slot" "1")) port)))
           (call-with-output-file (string-append directory "/bgp-neighbors.txt")
             (lambda (port)
               (display "group PROMETEY\n" port)
               (display #$(juniper-command host '("cli" "show" "bgp" "neighbor" "85.235.192.226"))
                        port)
               (display #$(juniper-command host '("cli" "show" "bgp" "neighbor" "85.235.198.236"))
                        port)
               (display "group DATAIX\n" port)
               (display #$(juniper-command host '("cli" "show" "bgp" "neighbor" "178.18.224.100"))
                        port)
               (display #$(juniper-command host '("cli" "show" "bgp" "neighbor" "178.18.227.100"))
                        port)
               (display "group SERVICE-PIPE\n" port)
               (display #$(juniper-command host '("cli" "show" "bgp" "neighbor" "10.70.0.21"))
                        port)
               (display #$(juniper-command host '("cli" "show" "bgp" "neighbor" "10.70.0.25"))
                        port)
               (display "group IBGP-OF\n" port)
               (display #$(juniper-command host '("cli" "show" "bgp" "neighbor" "10.10.3.100"))
                        port)
               (display "group IBGP-DH-BACKUP\n" port)
               (display #$(juniper-command host '("cli" "show" "bgp" "neighbor" "10.10.1.102"))
                        port)
               (display "group IBGP-DH\n" port)
               (display #$(juniper-command host '("cli" "show" "bgp" "neighbor" "10.10.0.3"))
                        port)
               (display "group IBGP-BORDER\n" port)
               (display #$(juniper-command host '("cli" "show" "bgp" "neighbor" "10.10.0.1"))
                        port)))
           (call-with-output-file (string-append directory "/bgp-route.txt")
             (lambda (port)
               (display #$(juniper-command host '("cli" "show" "route" "0.0.0.0/0" "detail"))
                        port)))
           (call-with-output-file (string-append directory "/bgp-route.txt")
             (lambda (port)
               (display "group PROMETEY\n" port)
               (display #$(juniper-command host '("show" "route" "advertising-protocol" "bgp" "85.235.192.226"))
                        port)
               (display #$(juniper-command host '("show" "route" "advertising-protocol" "bgp" "85.235.198.236"))
                        port))))))))

(define (juniper-configuration->vc host)
  (program-file
   (string-append "juniper-configuration-to-version-control-" host)
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (guix build utils))
         (invoke #$(juniper-configuration->file host))
         (with-directory-excursion #$%ansible-state-directory
           (invoke "git" "add" "--all")
           (invoke "git" "commit" "--message=Update."))))))

(define juniper-configuration->vc-sr1-mr13-14.intr
  (juniper-configuration->vc "sr1-mr13-14.intr"))

(define juniper-configuration->vc-sr1-dh507-508.intr
  (juniper-configuration->vc "sr1-dh507-508.intr"))

(define (juniper-mcron-jobs config)
  (list
   #~(job
      '(next-hour '(21))
      #$(run-with-store (open-connection)
          (lower-object juniper-configuration->vc-sr1-mr13-14.intr)))
   #~(job
      '(next-hour '(22))
      #$(run-with-store (open-connection)
          (lower-object juniper-configuration->vc-sr1-dh507-508.intr)))))

(define juniper-service-type
  (service-type
   (name 'juniper)
   (extensions
    (list (service-extension home-mcron-service-type
                             juniper-mcron-jobs)))
   (description
    "Periodically run Juniper configuration dump.")
   (default-value '())))
