;; GuixSD configuration file for the desktop machine.
;; Copyright © 2024, 2025, 2026 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

(define-module (wugi manifests notebook)
  #:use-module (srfi srfi-26)
  #:use-module (wugi manifests wm)
  #:use-module (wugi utils)
  #:export (%notebook-manifest))

(define (%notebook-manifest)
  (combined-manifest-from-files
   (append (map (cut string-append %distro-directory "/wugi/manifests/" <>)
                '("notebook-packages.scm"
                  "deprecated.scm"
                  "desktop.scm"
                  "dotfiles.scm"
                  "emacs.scm"
                  "telegram.scm"))
           (list manifest-wm))))
