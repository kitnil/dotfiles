(define-module (fiore modules hosts)
  #:use-module (fiore modules utils)
  #:use-module (gnu services networking)
  #:use-module (gnu system)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:export (%magnolia-ip-address
            fiore-hosts-file))

(define %magnolia-ip-address "192.168.105.120")

(define (prefix-local-host-aliases prefix
                                   host-name
                                   domain
                                   ip-addresses)
  (string-join (map (lambda (x)
                      (string-append (string-join x " ")
                                     "." host-name domain))
                    (cartesian-product ip-addresses prefix))
               "\n"))

(define (serialize-hosts lst)
  (string-join (map (match-lambda
                      ((ip-address . canonical-hostname)
                       (format #f "~a ~a"
                               ip-address canonical-hostname)))
                    lst)
               "\n"))

(define (fiore-hosts-file host-name)
  (plain-file "hosts"
              (string-append
               (local-host-aliases host-name)
               (prefix-local-host-aliases
                '("cgit" "git" "guix" "www"
                  "natsu" "torrent" "print"
                  "zabbix")
                host-name ".local"
                (list %magnolia-ip-address))
               "\n"
               (prefix-local-host-aliases
                '("cgit" "anongit" "guix" "alerta" "weblog")
                "duckdns" ".org"
                (list %magnolia-ip-address))
               "\n"
               (prefix-local-host-aliases
                '("zabbix" "cerberus" "grafana")
                "" "intr"
                (list %magnolia-ip-address))
               "\n\n"
               (serialize-hosts
                '(("192.168.100.1" . "router.local")
                  ("192.168.105.1" . "switch.local")
                  ("192.168.105.120" . "magnolia")
                  ("192.168.105.112" . "clover")
                  ("192.168.105.123" . "ansible")
                  ("192.168.105.124" . "ajenti")
                  ("192.168.105.124" . "wordpress.local")))
               "\n\n"
               %facebook-host-aliases)))
