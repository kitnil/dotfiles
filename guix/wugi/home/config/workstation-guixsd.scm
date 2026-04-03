(define-module (wugi home config workstation-guixsd)
  #:use-module (wugi home services desktop)
  #:use-module (wugi home config workstation)
  #:export (%workstation-guixsd-home-environment))

(define (%workstation-guixsd-home-environment)
  (home-environment
   (inherit (%workstation-home-environment))
   (service home-wayvnc-service-type
            (wayvnc-configuration
             (environment-variables '("WAYLAND_DISPLAY=wayland-1"))))))
