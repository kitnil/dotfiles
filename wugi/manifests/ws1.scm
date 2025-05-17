;; GuixSD configuration file for the desktop machine.
;; Copyright © 2018, 2019, 2025 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

(define-module (wugi manifests ws1)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module ((guix ui) #:select (make-user-module))
  #:use-module (guix profiles)
  #:use-module (wugi utils)
  #:export (%ws1-manifest))

;; The composite module that combines everything from the public modules.
;; Origin <https://lists.gnu.org/archive/html/help-guix/2018-10/msg00040.html>.

(define (load-manifest file)
  ;; Load manifest file in a fresh module with necessary imports.
  (let ((module (make-user-module '((guix profiles) (gnu)))))
    (save-module-excursion
     (lambda _
       (set-current-module module)
       (load (canonicalize-path file))))))

(define (combined-manifest-from-files files)
  (fold (lambda (file combined)
          (manifest-add combined
                        (manifest-entries (load-manifest file))))
        (manifest '())
        files))

(define (%ws1-manifest)
  (combined-manifest-from-files
   (map (cut string-append %distro-directory "/wugi/manifests/" <>)
        '("emacs.scm" "guix-collection.scm" "wigust.scm"))))
