(setq guix-find-file-function #'org-open-file)


;;;
;;; Debbugs
;;;

(defun wi-debbugs-gnu-guix ()
  "List Guix bugs on debbugs.gnu.org."
  (interactive)
  (debbugs-gnu '("serious" "important" "normal") '("guix")))

(defun wi-debbugs-gnu-guix-patches ()
  "List Guix patches on debbugs.gnu.org."
  (interactive)
  (debbugs-gnu '("serious" "important" "normal") '("guix-patches")))


;;;
;;; IRC
;;;

(defcustom guix-irc-log-url "https://gnunet.org/bot/log/guix"
  "URL to IRC #guix channel log."
  :type 'string
  :group 'guix)

(defun guix-irc-open-log ()
  "Open IRC #guix channel log."
  (interactive)
  (browse-url guix-irc-log-url))


;;;
;;; Version control
;;;

(defvar wi-guix-git-directory (expand-file-name "~/src/guix"))
(defun wi-magit-show-commit-guix (commit)
  "Show a Git `commit' from the Guix checkout.

If no commit hash provides, show a commit from hash at current point."
  (interactive (list (read-string "Commit: " nil nil (word-at-point))))
  (let ((default-directory wi-guix-git-directory))
    (magit-show-commit commit)))

(defun wi-magit-find-file-guix (commit file)
  "Show a `file' from Git `commit' in the Guix checkout."
  (interactive "sCommit: \nsFile: ")
  (let ((default-directory wi-guix-git-directory))
    (magit-find-file commit file)))

(defun wi-set-guix-directory (directory)
  "Set a `GUIX-DIRECTORY' path."
  (interactive "DDirectory: ")
  (setq guix-directory directory))


;;;
;;; CI
;;;

(defcustom guix-hydra-script "~/src/hello-guile/package.scm"
  "Script to get package names appropriate for Guix Hydra."
  :group 'guix-hydra)

(defun guix-hydra-packages (packages)
  "Return a list of PACKAGES appropriate for Guix Hydra."
  (remove ""
          (split-string (shell-command-to-string
                         (mapconcat 'identity
                                    (append (list (expand-file-name guix-hydra-script))
                                            packages)
                                    " "))
                        "\n")))

(defun guix-hydra-packages-browse (packages)
  "Open a WEB browser at Guix Hydra for PACKAGES."
  (interactive "sPackages (space separated): ")
  (mapc (lambda (package)
          (engine/search-guix-hydra-job package))
        (guix-hydra-packages (split-string packages " "))))

(setq guix-read-package-name-function
      #'guix-read-package-name-at-point)

(defun wi-guix-hydra-latest-builds (number)
  "Return a NUMBER of latest builds on Hydra."
  (interactive
   (list (read-number "Number of latest builds: " 64)))
  (flet ((guix-hydra-latest-builds-custom ()
          (guix-hydra-latest-builds number :project 'gnu :system "x86_64-linux")))
    (if current-prefix-arg
        (let ((guix-hydra-url "https://berlin.guixsd.org"))
          (funcall #'guix-hydra-latest-builds-custom))
      (funcall #'guix-hydra-latest-builds-custom))))

(defcustom guix-substitute-servers
  '("https://berlin.guixsd.org/" "https://hydra.gnu.org/")
  "List of Guix substitute servers."
  :type '(repeat string)
  :group 'guix)

(defun guix-substitute-servers-narinfo (hash)
  "Download a narinfo for HASH from Guix servers."
  (mapcar (lambda (server)
            (mapconcat 'identity
                       (list "wget" "-q" "-O" "-"
                             (concat server hash ".narinfo"))
                       " "))
          guix-substitute-servers))

(with-eval-after-load 'build-farm-url
  (add-to-list 'build-farm-url-alist
               '("http://cuirass.tld" . cuirass)))

(defvar bui-rgrep-directory
  "~/.local/share/chezmoi/dotfiles/fiore/manifests/"
  "Directory to search for a package in `bui-rgrep-manifests' procedure.")

(defun bui-rgrep-manifests ()
  "Invoke rgrep in `bui-rgrep-directory'."
  (interactive)
  (rgrep (substring-no-properties (aref (tabulated-list-get-entry) 0))
         "*.scm"
         (expand-file-name bui-rgrep-directory)))

(with-eval-after-load 'build-farm
  (defun wi-build-farm (job)
    "Wrapper for `build-farm' procedure.

Produces URL as https://ci.guix.info/api/latestbuilds?nr=10&jobset=guix-master&job=opam-2.0.1&system=x86_64-linux"
    (interactive (list (guix-read-package-name)))
    (let ((build-farm-url "https://ci.guix.info")
          (number 3)
          (job (if current-prefix-arg
                   (string-trim-right (shell-command-to-string (format "guix-package-version %s" job)))
                 job)))
      (apply #'build-farm-get-display
             build-farm-url 'build 'latest number (list :project nil
                                                        :jobset (if current-prefix-arg nil "guix-master")
                                                        :job (concat job ".x86_64-linux")
                                                        :system nil)))))

(defun wi-guix-download (url)
  "Download URL with a \"guix download\" shell command."
  (interactive "sDownload URL: ")
  (insert
   (shell-command-to-string
    (concat "guix download " url
            " 2>/dev/null" "| tail -n 1" "| tr -d '\n'"))))


;;;
;;; Project
;;;

(defun wi-compile-guix (directory)
  ""
  (interactive "DDirectory: ")
  (require 'compile)
  (mapc 'kill-process compilation-in-progress)
  (compile
   (format
    "cd %s; guix environment --pure guix --ad-hoc help2man guile-sqlite3 guile-gcrypt -- make -j4 -k"
    directory)))

(defun wi-copy-cgit-guix-path (path)
  "Copy cgit guix path to kill ring"
  (interactive "sPath: ")
  (kill-new (concat "https://git.savannah.gnu.org/cgit/guix.git/tree/"
                    path)))

(defun projectile-run-shell-guix ()
  (interactive)
  (projectile-run-shell)
  (font-lock-mode)
  (guix-build-log-minor-mode))


;;;
;;; Snippets
;;;

(defun guix-insert-copyright ()
  (interactive)
  (insert (format ";;; Copyright © %s %s\n"
                  (format-time-string "%Y")
                  (wi-fullname-and-email))))

(define-auto-insert
  (rx "package" (one-or-more (or alphanumeric "-")) ".scm" line-end)
  ["guix/gnu/packages/package" yas-expand-current-buffer])

(define-auto-insert
  (rx "gnu/services/" (one-or-more (or alphanumeric "-")) ".scm" line-end)
  ["guix/gnu/services/service" yas-expand-current-buffer])

(define-auto-insert
  (rx "gnu/tests/" (one-or-more (or alphanumeric "-")) ".scm" line-end)
  ["guix/gnu/tests/test" yas-expand-current-buffer])

(define-auto-insert
  (rx "vm" (one-or-more (or alphanumeric "-")) ".scm" line-end)
  ["guix/gnu/system/examples/vm-inherit-image" yas-expand-current-buffer])

