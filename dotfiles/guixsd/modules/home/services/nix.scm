(define-module (home services nix)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (home config)
  #:export (home-nix-service))

(define home-nix-service
  (simple-service 'nix-config
                  home-files-service-type
                  (list `(".config/nix/repl.nix" ,(local-file (string-append %project-directory "/dot_config/nix/repl.nix")))
                        `(".config/nix/nix.conf" ,(local-file (string-append %project-directory "/dot_config/nix/nix.conf")))
                        `(".config/nix/registry.json" ,(local-file (string-append %project-directory "/dot_config/nix/registry.json")))
                        `(".config/nixpkgs/config.nix" ,(local-file (string-append %project-directory "/dot_config/nixpkgs/config.nix"))))))
