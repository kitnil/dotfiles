;; GuixSD configuration file for the desktop machine.
;; Copyright © 2018, 2019, 2020, 2021, 2022, 2023, 2024 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

(use-modules (srfi srfi-26)
             (utils))

(define %source-dir
  (dirname (current-filename)))

(combined-manifest-from-files
 (map (cut string-append %source-dir "/" <>)
      '("deprecated.scm"
        "desktop.scm"
        "emacs.scm"
        "guix-collection.scm"
        "majordomo.scm"
        "nonguix.scm"
        "wigust.scm"
        "dotfiles.scm"
        "kubernetes.scm"
        "obs.scm"
        "obs-extra.scm"
        "python.scm"
        "telegram.scm"
        "wm.scm")))
