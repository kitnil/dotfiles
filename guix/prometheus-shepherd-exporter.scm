;; This Guile module provides a Prometheus exporter for the GNU Shepherd.
;;
;; Build command:
;; guix build -L . -e '(@ (prometheus-shepherd-exporter) prometheus-shepherd-exporter)'

(define-module (prometheus-shepherd-exporter)
  #:use-module (gnu packages guile-xyz)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:export (prometheus-shepherd-exporter))

(define prometheus-shepherd-exporter
  (program-file
   "prometheus-shepherd-exporter"
   (with-extensions (list guile-prometheus)
     (with-imported-modules (source-module-closure '((gnu services herd)
                                                     (prometheus)))
       #~(begin
           (use-modules (gnu services herd)
                        (prometheus)
                        (srfi srfi-1)
                        (web request)
                        (web response)
                        (web server)
                        (web uri))

           (define my-registry
             (make-metrics-registry #:namespace "shepherd"))

           (define request-counter
             (make-counter-metric my-registry "requests_count"))

           (define service-status-gauge
             (make-gauge-metric my-registry "service_status"))

           (define %exclude-services
             (append '(host-name
                       kernel-module-loader
                       kres-cache-gc
                       sysctl
                       term-console
                       user-homes)
                     '(lvm-thin)))

           (define (request-handler request body)
             (metric-increment request-counter)
             (for-each (lambda (service)
                         (metric-set service-status-gauge
                                     (cond
                                      ((member
                                        (live-service-canonical-name service)
                                        %exclude-services)
                                       2)
                                      ((live-service-running service)
                                       1)
                                      (else
                                       0))
                                     #:label-values
                                     `((service . ,(live-service-canonical-name service)))))
                       (current-services))
             (if (equal? (split-and-decode-uri-path (uri-path (request-uri request)))
                         '("metrics"))
                 (values '((content-type . (text/plain)))
                         (lambda (port)
                           (write-metrics my-registry port)))
                 (values '((content-type . (text/plain)))
                         (lambda (port)
                           (display "Incrementing metric\n" port)))))

           (run-server request-handler 'http
                       '(#:port 9180)))))))
