(define-module (fiore modules net)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (gnu packages linux)
  #:export (ip
            define-network-interface))

(define-record-type* <network-device>
  network-device make-network-device
  network-device?
  (name network-device-name) ;string
  (address network-device-address)) ;string

(define (ip command)
  #~(format #f "~a ~a"
            #$(file-append iproute "/sbin/iptables") command))

(define-syntax define-network-interface
  (syntax-rules ()
    "Documentation."
    ((_ name service-start service-stop (fields values) ...)
     (begin
       (define name
         (network-device (fields values) ...))
       (define service-start
         #~(and #$(ip (format #f "tuntap add ~a mode tap"
                              (network-device-name name)))
                #$(ip (format #f "address add ~a dev ~a"
                              (network-device-address name)
                              (network-device-name name)))
                #$(ip (format #f "link set ~a up"
                              (network-device-name name)))))
       (define service-stop
         #~(and #$(ip (format #f "link set ~a down"
                              (network-device-name name)))
                #$(ip (format #f "address delete ~a dev ~a"
                              (network-device-address name)
                              (network-device-name name)))
                #$(ip (format #f "tuntap delete ~a mode tap"
                              (network-device-name name)))))
       (simple-service (string->symbol (string-append "networking-"
                                                      (network-device-name name)))
                       shepherd-root-service-type
                       (list
                        (shepherd-service
                         (documentation (format #f "Bring up ~a networking interface."
                                                (network-device-name name)))
                         (provision '(name))
                         (requirement '(networking))
                         (start #~(lambda _
                                    #$service-start))
                         (respawn? #f)
                         (stop #~(lambda _
                                   #$service-stop)))))))))
