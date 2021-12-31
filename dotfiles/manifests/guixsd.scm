;; GuixSD configuration file for the desktop machine.
;; Copyright © 2018, 2019, 2020, 2021 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

(use-modules (srfi srfi-1)
             (srfi srfi-26)
             ((guix ui) #:select (make-user-module))
             (guix profiles))

(add-to-load-path "/home/oleg/.local/share/chezmoi/dotfiles/manifests")
(use-modules (deprecated))

;; The composite module that combines everything from the public modules.
;; Origin <https://lists.gnu.org/archive/html/help-guix/2018-10/msg00040.html>.

(define %source-dir (dirname (current-filename)))

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
        (packages->manifest (list firefox openssh tigervnc-client))
        files))

(combined-manifest-from-files
 (map (cut string-append %source-dir "/" <>)
      '("desktop.scm"
        "dotfiles.scm"
        "emacs.scm"
        "guix-collection.scm"
        "majordomo.scm"
        "wigust.scm")))
