(define-module (fiore modules network-devices)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (gnu packages linux)
  #:export (network-device
            network-device-start-stop))

(define-record-type* <tap-device>
  tap-device make-tap-device
  tap-device?
  (name tap-device-name) ;string
  (address tap-device-address)) ;string

(define* (network-device-start-stop device #:key (start? #t))
  (match device
    (($ <network-device> name object mode address mask)
     #~(let ((ip
              (lambda (str)
                (zero? (system (format #f "~a ~a"
                                       #$(file-append iproute "/sbin/ip")
                                       str))))))
         (and (ip #$(format #f "~a ~a ~a mode ~a"
                            object (if start? "add" "delete") name mode))
              (ip #$(format #f "address ~a ~a/~a dev ~a"
                          (if start? "add" "delete") address mask name))
              (ip #$(format #f "link set ~a ~a"
                          name (if start? "up" "down"))))))))

