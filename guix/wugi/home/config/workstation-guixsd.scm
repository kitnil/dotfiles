(define-module (wugi home config workstation-guixsd)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (wugi home config workstation)
  #:use-module (wugi home services desktop)
  #:export (%workstation-guixsd-home-environment))

(define (%workstation-guixsd-home-environment)
  (let ((workstation-home (%workstation-home-environment)))
    (home-environment
     (inherit workstation-home)
     (services
      (append
       (list
        (service home-wayvnc-service-type
                 (wayvnc-configuration
                  (environment-variables '("WAYLAND_DISPLAY=wayland-1")))))
       (home-environment-services workstation-home))))))
