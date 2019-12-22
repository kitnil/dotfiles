(define-module (fiore vm)
  #:use-module (gnu)
  #:use-module (gnu services base)
  #:use-module (gnu services databases)
  #:use-module (gnu services networking)
  #:use-module (gnu services rsync)
  #:use-module (guix gexp)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages databases)
  #:use-module (gnu services dns)
  #:use-module (gnu services ssh)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ssh)
  #:use-module (gnu services web)
  #:use-module (gnu packages linux)
  #:use-module (wigust packages monitoring)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:use-module (gnu services shepherd)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:export (%vm))

(define %vm
  (let ((os (load "/home/natsu/src/guix/gnu/system/examples/vm-image.tmpl")))
    (operating-system
      (inherit os)
      (timezone "Europe/Moscow")
      (kernel-arguments '("console=ttyS0"))
      (users (cons (user-account
                    (name "natsu")
                    (comment "Oleg Pykhalov")
                    (group "users")
                    (supplementary-groups '("wheel" "netdev"
                                            "audio" "video"))
                    (home-directory "/home/natsu")
                    (password (crypt "salt" "bar")))
                   (operating-system-users os)))
      (services (cons* 
                 (simple-service 'ddclient-secret shepherd-root-service-type
                                 (list
                                  (shepherd-service
                                   (provision '(ddclient-secret))
                                   (requirement '())
                                   (start #~(lambda _ (with-output-to-file "/etc/ddclient/secrets.conf" (lambda _ (display "use=web, web=checkip.dyndns.org/, web-skip='IP Address'
protocol=duckdns,                \
password=***REMOVED***         \
tail
")))))
                                   (respawn? #f))))
                 (service ddclient-service-type)
                 (dhcp-client-service)
                 (operating-system-user-services os)))
      (packages (cons* curl knot nss-certs openssh
                       ncurses ;reset
                       tcpdump
                       strace
                       (operating-system-packages os)))
      (name-service-switch %mdns-host-lookup-nss))))

%vm
