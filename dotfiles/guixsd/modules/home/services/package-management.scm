(define-module (home services package-management)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages package-management)
  #:use-module (gnu home services mcron)
  #:use-module (ice-9 match)
  #:export (nix-delete-generations-service-type
            nix-delete-generations-configuration

            guix-delete-generations-service-type
            guix-delete-generations-configuration

            nix-build-service-type
            nix-build-configurations
            nix-build-configuration))


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

(define %home
  (and=> (getenv "HOME")
         (lambda (home)
           home)))

(add-to-load-path (string-append %home "/.local/bin"))
(use-modules (mjru-github-projects))

(define-record-type* <nix-build-configurations>
  nix-build-configurations make-nix-build-configurations
  nix-build-configurations?
  (configurations nix-build-configurations-configurations ;<nix-build-configuration>
                  (default '()))
  (schedule       nix-build-configurations-shedule        ;mcron specification
                  (default '(next-hour '(21))))
  (nix            nix-build-configuration-nix             ;<package>
                  (default nix)))

(define-record-type* <nix-build-configuration>
  nix-build-configuration make-nix-build-configuration
  nix-build-configuration?
  (name         nix-build-configuration-name        ;string
                (default #f))
  (git-project  nix-build-configuration-git-project ;<git-project>
                (default #f)))

(add-to-load-path (string-append %home "/.local/bin"))
(use-modules (mjru-github-projects))

(define (nix-build-mcron-jobs config)
  (list
   #~(job
      '#$(nix-build-configurations-shedule config)
      #$(program-file
         "nix-build"
         (with-imported-modules '((ice-9 format)
                                  (ice-9 match)
                                  (guix build utils))
           #~(begin
               (use-modules (ice-9 match)
                            (guix build utils))

               (add-to-load-path #$(string-append %home "/.local/bin"))
               (use-modules (mjru-github-projects))

               #$@(map
                   (match-lambda
                     (($ <nix-build-configuration> name git-project)
                      (match git-project
                        (($ (@@ (mjru-github-projects) <git-project>) _ _ output)
                         #~(with-directory-excursion #$output
                             (let ((command (list (string-append #$(@@ (gnu packages package-management) nix) "/bin/nix-shell")
                                                  "--run"
                                                  #$(string-append "nix build --no-link --builders 'ssh://nixos.intr x86_64-linux' .#nixosConfigurations."
                                                                   name
                                                                   ".config.system.build.toplevel"))))
                               (format #t "Running `~a'.~%" (string-join command))
                               (apply system* command)))))))
                   (nix-build-configurations-configurations config))))))))

(define nix-build-service-type
  (service-type
   (name 'nix-build)
   (extensions
    (list (service-extension home-mcron-service-type
                             nix-build-mcron-jobs)))
   (description "Periodically run nix build.")
   (default-value '())))
