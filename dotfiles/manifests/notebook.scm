;; GuixSD configuration file for the desktop machine.
;; Copyright © 2024 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

(use-modules (srfi srfi-26)

             (manifests wm)
             (utils))

(define %source-dir
  (dirname (current-filename)))

(combined-manifest-from-files
 (append (map (cut string-append %source-dir "/" <>)
              '("notebook-packages.scm"
                "deprecated.scm"
                "desktop.scm"
                "dotfiles.scm"
	        "emacs.scm"
                "obs.scm"
                "telegram.scm"))
         (list manifest-wm)))
