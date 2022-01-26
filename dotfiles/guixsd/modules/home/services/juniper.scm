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
      (use-modules (ice-9 rdelim)
                   (ice-9 popen))
      (let* ((port (open-pipe* OPEN_READ #$%connect-program #$host #$@command))
             (output (read-string port)))
        (close-port port)
        output)))

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
           (call-with-output-file (string-append directory "/chassis-pic.txt")
             (lambda (port)
               (display #$(juniper-command host '("cli" "show" "chassis" "pic" "pic-slot" "0" "fpc-slot" "0")) port)
               (display #$(juniper-command host '("cli" "show" "chassis" "pic" "pic-slot" "0" "fpc-slot" "1")) port)
               (display #$(juniper-command host '("cli" "show" "chassis" "pic" "pic-slot" "1" "fpc-slot" "0")) port)
               (display #$(juniper-command host '("cli" "show" "chassis" "pic" "pic-slot" "1" "fpc-slot" "1")) port))))))))

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
