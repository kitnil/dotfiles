(define-module (home services juniper)
  #:use-module (gnu home services)
  #:use-module (gnu home services mcron)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:export (juniper-service-type))

(define %home
  (and=> (getenv "HOME")
         (lambda (home)
           home)))

(define %state-directory
  (string-append %home "/ansible-out/files"))

(define %connect-program
  (string-append %home "/.local/bin/connect"))

(define (juniper-command host command)
  #~(begin
      (use-modules (ice-9 rdelim)
                   (ice-9 popen))
      (let* ((port (open-pipe* OPEN_READ #$%connect-program #$host #$@command))
             (output (read-string port)))
        (close-port port)
        output)))

(define (juniper-show-configuration host)
  (juniper-command host '("cli" "show" "configuration")))

(define (juniper-show-dhcp host)
  (juniper-command host '("cli" "show" "system" "services" "dhcp" "binding")))

(define (juniper-configuration->file host)
  (program-file
   "juniper-show-configuration-program"
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (guix build utils))
         (let* ((directory
                 (string-append #$%state-directory "/" #$host "/config"))
                (file (string-append directory "/juniper.conf")))
           (mkdir-p directory)
           (call-with-output-file (string-append directory "/juniper.conf")
             (lambda (port)
               (display #$(juniper-show-configuration host) port)))
           (call-with-output-file (string-append directory "/dhcp.txt")
             (lambda (port)
               (display #$(juniper-show-dhcp host) port))))))))

(define (juniper-configuration->vc host)
  (program-file
   "juniper-configuration-to-version-control"
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (guix build utils))
         (invoke #$(juniper-configuration->file host))
         (with-directory-excursion #$%state-directory
           (invoke "git" "add" "--all")
           (invoke "git" "commit" "--message=Update.")
           ;; (invoke "git" "push")
           )))))

(define (juniper-mcron-jobs config)
  (list
   #~(job
      '(next-hour '(20))
      #$(run-with-store (open-connection)
          (lower-object
           (juniper-configuration->vc "br1-mr14.intr"))))
   #~(job
      '(next-hour '(21))
      #$(run-with-store (open-connection)
          (lower-object
           (juniper-configuration->vc "sr1-mr13-14.intr"))))
   #~(job
      '(next-hour '(22))
      #$(run-with-store (open-connection)
          (lower-object
           (juniper-configuration->vc "sr1-dh507-508.intr"))))))

(define juniper-service-type
  (service-type
   (name 'juniper)
   (extensions
    (list (service-extension home-mcron-service-type
                             juniper-mcron-jobs)))
   (description
    "Periodically run Juniper configuration dump.")
   (default-value '())))
