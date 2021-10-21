(define-module (home services package-management)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages package-management)
  #:use-module (gnu home services mcron)
  #:export (nix-delete-generations-service-type
            nix-delete-generations-configuration))

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
