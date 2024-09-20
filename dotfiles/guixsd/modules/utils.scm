(define-module (utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module ((guix ui) #:select (make-user-module))
  #:use-module (guix profiles)
  #:export (combined-manifest-from-files))

;; The composite module that combines everything from the public modules.
;; Origin <https://lists.gnu.org/archive/html/help-guix/2018-10/msg00040.html>.

(define (load-manifest file)
  ;; Load manifest file in a fresh module with necessary imports.
  (let ((module (make-user-module '((guix profiles) (gnu)))))
    (save-module-excursion
     (lambda _
       (set-current-module module)
       (load (canonicalize-path file))))))

(define* (combined-manifest-from-files manifests)
  (fold (lambda (manifest combined)
          (if (manifest? manifest)
              (manifest-add combined
                            (manifest-entries manifest))
              (manifest-add combined
                            (manifest-entries (load-manifest manifest)))))
        (packages->manifest '())
        manifests))
