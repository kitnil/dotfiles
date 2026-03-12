;; GuixSD configuration file for the desktop machine.
;; Copyright © 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

(define-module (wugi manifests guixsd)
  #:use-module (srfi srfi-26)
  #:use-module (wugi manifests wm)
  #:use-module (wugi utils)
  #:export (%guixsd-manifest))

(define (%guixsd-manifest)
  (combined-manifest-from-files
   (append (map (cut string-append %distro-directory "/wugi/manifests/" <>)
                '("deprecated.scm"
                  "desktop.scm"
                  "emacs.scm"
                  "guix-collection.scm"
                  "majordomo.scm"
                  "wigust.scm"
                  "dotfiles.scm"
                  "kubernetes.scm"
                  "python.scm"
                  "telegram.scm"))
           (list manifest-wm))))
