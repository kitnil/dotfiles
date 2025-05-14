;; GuixSD configuration file for the desktop machine.
;; Copyright © 2024, 2025 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

(use-modules (srfi srfi-26)

             (manifests wm)
             (utils))

(define %source-dir
  (dirname (current-filename)))

(combined-manifest-from-files
 (append (map (cut string-append %source-dir "/" <>)
              '("pc0-packages.scm"
                "desktop.scm"
                "icons.scm"
                "dotfiles.scm"
                "emacs.scm"
                "deprecated.scm"
                "telegram.scm"
                "majordomo.scm"
                "notebook-packages.scm"
                "guile.scm"
                "obs.scm"
                ;; "ai.scm"
                ))
         (list manifest-wm)))
