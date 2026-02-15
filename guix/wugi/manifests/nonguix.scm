(define-module (wugi manifests nonguix)
  #:use-module (guix channels)
  #:use-module (guix inferior)
  #:use-module (guix profiles)
  #:use-module (guix store)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (wugi etc guix channels workstation)
  #:use-module (wugi utils)
  #:export (%nonguix-manifest))

(define (%nonguix-manifest)
  (define inferior
    (inferior-for-channels %channels-workstation
                           #:cache-directory "/home/oleg/.cache/guix/inferiors"))

  (define inferior-font-packages
    (inferior-eval `(begin
                      (use-modules (nongnu packages fonts)
                                   (srfi srfi-1))
                      (fold (lambda (package result)
                              (let ((id (object-address package)))
                                (hashv-set! %package-table id package)
                                (cons (list (package-name package)
                                            (package-version package)
                                            id)
                                      result)))
                            '()
                            (list font-awesome-nonfree
                                  font-microsoft-andale-mono
                                  font-microsoft-arial
                                  font-microsoft-arial-black
                                  font-microsoft-comic-sans-ms
                                  font-microsoft-couirer-new
                                  font-microsoft-courier-new
                                  font-microsoft-georgia
                                  font-microsoft-impact
                                  font-microsoft-times-new-roman
                                  font-microsoft-trebuchet-ms
                                  font-microsoft-verdana
                                  font-microsoft-web-core-fonts
                                  font-microsoft-webdings
                                  font-ubuntu)))
                   inferior))

  (define inferior-k8s-packages
    (inferior-eval `(begin
                      (use-modules (nongnu packages k8s)
                                   (srfi srfi-1))
                      (fold (lambda (package result)
                              (let ((id (object-address package)))
                                (hashv-set! %package-table id package)
                                (cons (list (package-name package)
                                            (package-version package)
                                            id)
                                      result)))
                            '()
                            (list kubectl)))
                   inferior))

  (packages->manifest (append (map (match-lambda
                                     ((name version id)
                                      ((@@ (guix inferior) inferior-package)
                                       inferior name version id)))
                                   inferior-font-packages)
                              (map (match-lambda
                                     ((name version id)
                                      ((@@ (guix inferior) inferior-package)
                                       inferior name version id)))
                                   inferior-k8s-packages))))
