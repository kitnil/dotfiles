(define-module (home services networking)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (home config)
  #:use-module (home services cisco)
  #:use-module (home services juniper)
  #:use-module (home services h3c)
  #:export (home-networking-service))

(define home-networking-service
  (simple-service 'networking-config
                  home-files-service-type
                  (append
                   (let ((configurations (list `(".local/bin/juniper-configuration-vc-sr1-mr13-14.intr"
                                                 ,juniper-configuration->vc-sr1-mr13-14.intr)
                                               `(".local/bin/juniper-configuration-vc-sr1-dh507-508.intr"
                                                 ,juniper-configuration->vc-sr1-dh507-508.intr)
                                               `(".local/bin/juniper-configuration-vc-sw2-mr13.intr"
                                                 ,juniper-configuration->vc-sw2-mr13.intr)
                                               `(".local/bin/cisco-configuration-vc-sw1-dh507.intr"
                                                 ,cisco-configuration->vc-sw1-dh507.intr)
                                               `(".local/bin/cisco-configuration-vc-sw2-dh507.intr"
                                                 ,cisco-configuration->vc-sw2-dh507.intr)
                                               `(".local/bin/cisco-configuration-vc-sw1-dh508.intr"
                                                 ,cisco-configuration->vc-sw1-dh508.intr)
                                               `(".local/bin/cisco-configuration-vc-sw2-dh508.intr"
                                                 ,cisco-configuration->vc-sw2-dh508.intr)
                                               `(".local/bin/cisco-configuration-vc-sw1-mr11.intr"
                                                 ,cisco-configuration->vc-sw1-mr11.intr)
                                               `(".local/bin/cisco-configuration-vc-sw1-mr12.intr"
                                                 ,cisco-configuration->vc-sw1-mr12.intr)
                                               `(".local/bin/cisco-configuration-vc-sw2-mr12.intr"
                                                 ,cisco-configuration->vc-sw2-mr12.intr)
                                               `(".local/bin/cisco-configuration-vc-sw3-mr13.intr"
                                                 ,cisco-configuration->vc-sw3-mr13.intr)
                                               `(".local/bin/cisco-configuration-vc-sw1-mr14.intr"
                                                 ,cisco-configuration->vc-sw1-mr14.intr)
                                               `(".local/bin/cisco-configuration-vc-sw2-mr14.intr"
                                                 ,cisco-configuration->vc-sw2-mr14.intr)
                                               `(".local/bin/h3c-configuration-vc-sw4-mr14.intr"
                                                 ,h3c-configuration->vc-sw4-mr14.intr)
                                               `(".local/bin/h3c-configuration-vc-sw4-mr13.intr"
                                                 ,h3c-configuration->vc-sw4-mr13.intr)
                                               `(".local/bin/h3c-configuration-vc-sw4-mr12.intr"
                                                 ,h3c-configuration->vc-sw4-mr12.intr)
                                               `(".local/bin/h3c-configuration-vc-sw4-mr11.intr"
                                                 ,h3c-configuration->vc-sw4-mr11.intr))))
                     (append
                      (list `(".local/bin/mjru-networking-configuration-vc"
                              ,((@ (ice-9 match) match)
                                configurations
                                (((name programs) ...)
                                 (program-file
                                  "mjru-networking-configuration-vc"
                                  #~(begin
                                      (use-modules (ice-9 format))
                                      (for-each (lambda (program)
                                                  (format #t "Running `~a'...~%" program)
                                                  (system* program))
                                                '#$programs)))))))
                      configurations)))))
