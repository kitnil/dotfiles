(define-module (wugi utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (guix profiles)
  #:export (combined-manifest-from-files
            %distro-directory))

;; The composite module that combines everything from the public modules.
;; Origin <https://lists.gnu.org/archive/html/help-guix/2018-10/msg00040.html>.

(define* (combined-manifest-from-files manifests)
  (fold (lambda (manifest combined)
          (if (manifest? manifest)
              (manifest-add combined
                            (manifest-entries manifest))
              (manifest-add combined
                            (manifest-entries (load-manifest manifest)))))
        (packages->manifest '())
        manifests))

(define %distro-directory
  (dirname (canonicalize-path (search-path %load-path "wugi.scm"))))
