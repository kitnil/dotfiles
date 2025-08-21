;; Switch to a system:
;;     sudo -i /gnu/store/4qy1pq6qbkgpvhiknwyjmyy882h00kd4-switch-to-system.scm
;;
;; Upgrade shepherd services:
;;
;;     sudo guix repl -L guix <<< '(load "guix/dot_local/bin/docker-image-shepherd-upgrade-services.scm")'

(use-modules ((gnu services) #:select (sexp->system-provenance))
             ((guix inferior) #:select (inferior-exception? inferior-exception-arguments))
             ((guix platform) #:select (systems))
             ((guix self) #:select (make-config.scm))
             (gcrypt pk-crypto)
             (gnu system file-systems)
             (gnu system uuid)
             (gnu system)
             (guix derivations)
             (guix diagnostics)
             (guix gexp)
             (guix i18n)
             (guix memoization)
             (guix modules)
             (guix monads)
             (guix pki)
             (guix records)
             (guix remote)
             (guix scripts system reconfigure)
             (guix ssh)
             (guix store)
             (guix utils)
             (ice-9 format)
             (ice-9 match)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-19)
             (srfi srfi-26)
             (srfi srfi-34)
             (srfi srfi-35)
             (srfi srfi-9)
             (wugi system docker-image))

(define-syntax-rule (save-load-path-excursion body ...)
  "Save the current values of '%load-path' and '%load-compiled-path', run
BODY..., and restore them."
  (let ((path %load-path)
       (cpath %load-compiled-path))
    (dynamic-wind
       (const #t)
       (λ ()
         body ...)
       (λ ()
         (set! %load-path path)
         (set! %load-compiled-path cpath)))))

(define (local-eval exp)
  "Evaluate EXP, a G-Expression, in-place."
  (mlet* %store-monad ((lowered (lower-gexp exp))
                      (_ (built-derivations (lowered-gexp-inputs lowered))))
        (save-load-path-excursion
         (set! %load-path (lowered-gexp-load-path lowered))
         (set! %load-compiled-path (lowered-gexp-load-compiled-path lowered))
         (return (primitive-eval (lowered-gexp-sexp lowered))))))

(run-with-store (open-connection)
  (mlet %store-monad ((a (upgrade-shepherd-services local-eval (%docker-image))))
    (return (list a))))
