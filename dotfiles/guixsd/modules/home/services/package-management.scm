(define-module (home services package-management)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages package-management)
  #:use-module (gnu home services mcron)
  #:export (nix-delete-generations-service-type
            nix-delete-generations-configuration

            guix-delete-generations-service-type
            guix-delete-generations-configuration))


;;;
;;; Guix
;;;

(define-record-type* <guix-delete-generations-configuration>
  guix-delete-generations-configuration make-guix-delete-generations-configuration
  guix-delete-generations-configuration?
  (guix     guix-delete-generations-configuration-guix ;<package>
            (default guix))
  (period   guix-delete-generations-configuration-period ;string
            (default "1w"))
  (schedule guix-delete-generations-configuration-schedule ;mcron specification
            (default '(next-hour '(20)))))

(define (guix-delete-generations-mcron-jobs config)
  (let ((period (guix-delete-generations-configuration-period config))
        (guix-binary
         (file-append (guix-delete-generations-configuration-guix config)
                      "/bin/guix")))
    (list
     #~(job
        '#$(guix-delete-generations-configuration-schedule config)
        #$(program-file
           "guix-delete-generations"
           #~(begin
               (system* #$guix-binary "package"
                        (string-append "--delete-generations=" #$period))
               (system* #$guix-binary "pull"
                        (string-append "--delete-generations=" #$period))))))))

(define guix-delete-generations-service-type
  (service-type
   (name 'guix-delete-generations)
   (extensions
    (list (service-extension home-mcron-service-type
                             guix-delete-generations-mcron-jobs)))
   (description
    "Periodically delete Guix generations.")
   (default-value (guix-delete-generations-configuration))))


;;;
;;; Nix
;;;

(define-record-type* <nix-delete-generations-configuration>
  nix-delete-generations-configuration make-nix-delete-generations-configuration
  nix-delete-generations-configuration?
  (nix    nix-delete-generations-configuration-nix       ;<package>
          (default nix))
  (period   nix-delete-generations-configuration-period  ;string
            (default "7d"))
  (schedule nix-delete-generations-configuration-shedule ;mcron specification
            (default '(next-hour '(20)))))

(define (nix-delete-generations-mcron-jobs config)
  (list
   #~(job
      '#$(nix-delete-generations-configuration-shedule config)
      #$(program-file
         "nix-delete-generations"
         #~(begin
             (system* (string-append #$(nix-delete-generations-configuration-nix config)
                                     "/bin/nix-env")
                      "--delete-generations"
                      #$(nix-delete-generations-configuration-period config)))))))

(define nix-delete-generations-service-type
  (service-type
   (name 'nix-delete-generations)
   (extensions
    (list (service-extension home-mcron-service-type
                             nix-delete-generations-mcron-jobs)))
   (description
    "Periodically run nix-env --delete-generations.")
   (default-value (nix-delete-generations-configuration))))
