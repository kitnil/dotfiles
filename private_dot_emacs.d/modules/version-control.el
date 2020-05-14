
;;;
;;; Browse at remote
;;;

(setq browse-at-remote-remote-type-domains
      '(("gitlab.intr" . "gitlab")
        ("bitbucket.org" . "bitbucket")
        ("github.com" . "github")
        ("gitlab.com" . "gitlab")
        ("git.savannah.gnu.org" . "gnu")
        ("git.net-core.org" . "gitlab")))

;;;
;;; Git patch
;;;

;; List of Email addresses to send patches for `gitpatch-mail' command
(setq gitpatch-mail-database (list "guix-patches@gnu.org"))


;;;
;;; vc tools
;;;

(setq vc-follow-symlinks t)  ; Do not ask about following link in Git projects

(with-eval-after-load 'vc-git
  (let ((map vc-git-log-edit-mode-map))
    (define-key map (kbd "C-c /") 'hydra-dabbrev-expand/body)
    (define-key map (kbd "C-c l") 'vc-chlog))
  (let ((map vc-git-log-view-mode-map))
    (define-key map (kbd "s") 'magit-show-commit)))


;;;
;;; Git Gutter
;;;

(defun wi-git-gutter:stage-hunk ()
  "Stage this hunk like 'git add -p'."
  (interactive)
  (flet ((yes-or-no-p (action)
                      (y-or-n-p
                       (format "%s current hunk ? " action))))
    (git-gutter:query-action "Stage"
                             #'git-gutter:do-stage-hunk
                             #'git-gutter)))

(advice-add 'git-gutter:stage-hunk
            :override #'wi-git-gutter:stage-hunk)

(defun wi-git-gutter-refresh-visible-buffers ()
  "Refresh command `git-gutter-mode' on all visible command `git-gutter-mode' buffers."
  (dolist (buff (buffer-list))
    (with-current-buffer buff
      (when (and git-gutter-mode (get-buffer-window buff))
        (git-gutter-mode t)))))


;;;
;;; vc-chlog
;;;

(defun vc-chlog ()
  "Insert output of `vc-chlog'."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (insert (shell-command-to-string (mapconcat 'identity
                                                (list "vc-chlog"
                                                      "| sed 's/^[ \t]*//'"
                                                      "| tail +2")
                                                " ")))))


;;;
;;; GitLab
;;;

(with-eval-after-load 'gitlab-snip-helm
  (defun gitlab-snip-helm-auth (host user)
    (let ((secret (plist-get (nth 0
                                  (auth-source-search
                                   :host host
                                   :user user))
                             :secret)))
      (and (functionp secret)
           (funcall secret))))
  (setq gitlab-snip-helm-server "https://gitlab.com")
  (setq gitlab-snip-helm-user-token (gitlab-snip-helm-auth "gitlab.com/api/v4" "wigust^token"))
  (defun gitlab-intr-snippet ()
    (interactive)
    (let ((gitlab-snip-helm-server "https://gitlab.intr")
          (gitlab-snip-helm-user-token (gitlab-snip-helm-auth "gitlab.intr/api/v4" "pyhalov^token")))
      (gitlab-snip-helm-insert))))


;;;
;;; Magit
;;;

(defvar wi-src (expand-file-name "~/src"))

(defun wi-magit-status-dir (dir)
  "Open magit in DIR directory."
  (let ((default-directory (expand-file-name dir))
        (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1))
    (magit-status)))

(defvar magit-read-reuse-message-target "ORIG_HEAD")
(setq magit-read-reuse-message-target "HEAD")

(defun wi-magit-read-reuse-message (prompt &optional default)
  "Reuse message for Git commit."
  (magit-completing-read prompt (magit-list-refnames)
                         nil nil nil 'magit-revision-history
                         (or default
                             (and (magit-rev-verify
                                   magit-read-reuse-message-target)
                                  magit-read-reuse-message-target))))

(advice-add 'magit-read-reuse-message
            :override #'wi-magit-read-reuse-message)

;; TODO: This is slow down Emacs startup.
;; (magit-org-todos-autoinsert)

(setq magit-repository-directories
      (mapcar (lambda (dir)
                (cons dir 0))
              (f-directories wi-src)))
(setq magit-repository-directories-depth 0)
(setq magit-log-arguments '("--graph" "--color" "--decorate" "-n64"))
(setq magit-log-section-arguments (list "-n256" "--decorate"))

;; Use `magit-describe-section'
(defun wi-local-magit-initially-hide-unmerged (section)
  "Hide unmerged files in magit SECTION."
  (and (not magit-insert-section--oldroot)
       (or (eq (magit-section-type section) 'unpushed)
           (equal (magit-section-value section) "@{upstream}..")
           (eq (magit-section-type section) 'stashes)
           (equal (magit-section-value section) "refs/stash"))
       'hide))

(defun magit-init-bare (directory)
  "Initialize a bare Git repository.

If the directory is below an existing repository, then the user
has to confirm that a new one should be created inside.  If the
directory is the root of the existing repository, then the user
has to confirm that it should be reinitialized.

Non-interactively DIRECTORY is (re-)initialized unconditionally."
  (interactive
   (let ((directory (file-name-as-directory
                     (expand-file-name
                      (read-directory-name "Create repository in: ")))))
     (-when-let (toplevel (magit-toplevel directory))
       (setq toplevel (expand-file-name toplevel))
       (unless (y-or-n-p (if (file-equal-p toplevel directory)
                             (format "Reinitialize existing repository %s? "
                                     directory)
                           (format "%s is a repository.  Create another in %s? "
                                   toplevel directory)))
         (user-error "Abort")))
     (list directory)))
  ;; `git init' does not understand the meaning of "~"!
  (magit-call-git "init" "--bare"
                  (magit-convert-filename-for-git
                   (expand-file-name directory))))

(defun wi-git-init+add-remote+push (source destination)
  "Initialize bare Git repository in DESTINATION directory fetched from SOURCE.

Then add local remote pointing to DESTINATION directory.

And finally push branch master to local/master."
  (interactive
   (list
    (read-directory-name "Source directory: ")
    (if wi-git wi-git
      (read-directory-name "Destination directory: "))))
  (let ((destination
         (concat (directory-file-name destination)
                 "/" (file-name-base
                      (directory-file-name (vc-git-root source))))))
    (magit-init-bare destination)
    (magit-remote-add "local" (concat "file://" destination))
    (magit-push "master" "local/master" nil)))

;; TODO: Another way will be in a new release,
;; see <https://emacs.stackexchange.com/a/38782/15092>.
;; (add-to-list 'magit-section-initial-visibility-alist '(stashes . hide))

;; XXX: eq: Symbol’s function definition is void: magit-section-type
;; (add-hook 'magit-section-set-visibility-hook
;;           'wi-local-magit-initially-hide-unmerged)

(add-hook 'git-commit-mode-hook 'auto-fill-mode)

;; Origin <https://github.com/alphapapa/unpackaged.el#improved-magit-status-command>.
(defun unpackaged/magit-status ()
  "Open a `magit-status' buffer and close the other window so only Magit is visible.
If a file was visited in the buffer that was active when this
command was called, go to its unstaged changes section."
  (interactive)
  (let* ((buffer-file-path (when buffer-file-name
                             (file-relative-name buffer-file-name
                                                 (locate-dominating-file buffer-file-name ".git"))))
         (section-ident `((file . ,buffer-file-path) (unstaged) (status))))
    (magit-status)
    (delete-other-windows)
    (when buffer-file-path
      (goto-char (point-min))
      (cl-loop until (when (equal section-ident (magit-section-ident (magit-current-section)))
                       (magit-section-show (magit-current-section))
                       (recenter)
                       t)
               do (condition-case nil
                      (magit-section-forward)
                    (error (cl-return (magit-status-goto-initial-section-1))))))))

;; Origin <https://github.com/alphapapa/unpackaged.el>
(defhydra unpackaged/smerge-hydra
  (:color pink :hint nil :post (smerge-auto-leave))
  "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
  ("n" smerge-next)
  ("p" smerge-prev)
  ("b" smerge-keep-base)
  ("u" smerge-keep-upper)
  ("l" smerge-keep-lower)
  ("a" smerge-keep-all)
  ("RET" smerge-keep-current)
  ("\C-m" smerge-keep-current)
  ("<" smerge-diff-base-upper)
  ("=" smerge-diff-upper-lower)
  (">" smerge-diff-base-lower)
  ("R" smerge-refine)
  ("E" smerge-ediff)
  ("C" smerge-combine-with-next)
  ("r" smerge-resolve)
  ("k" smerge-kill-current)
  ("ZZ" (lambda ()
          (interactive)
          (save-buffer)
          (bury-buffer))
   "Save and bury buffer" :color blue)
  ("q" nil "cancel" :color blue))

(defun wi-magit-init (directory group)
  "Call `magit-init' and create GitLab repository in project DIRECTORY for GROUP."
  (interactive "DCreate repository in: \nsGroup: ")
  (magit-init directory)
  (let ((name (file-name-nondirectory (directory-file-name directory)))
        (buffer (get-buffer-create "*wi-magit-init*")))
    (call-process "gitlab" nil buffer nil "create_project" name
                   (format "{visibility: public, namespace_id: %s}"
                           group))
    (call-process "git" nil buffer nil "remote" "add" "origin"
                  (format "git@gitlab:~s/~s.git"
                          group name))))

(add-hook 'magit-diff-visit-file-hook
          '(lambda ()
             (when smerge-mode
               (unpackaged/smerge-hydra/body))))

(defmacro wi-define-magit-status-repo (name directory)
  `(defun ,(intern (concat "wi-magit-status-repo-"
                           (symbol-name name)))
       nil
     (interactive)
     (magit-status ,directory)))

(defmacro define-wi-browse-url-git-commit (repository directory commit-function)
  `(progn
     (defun ,(intern (concat "browse-url-" (symbol-name repository) "-git-commit")) (url &optional new-window)
       (concat "Show a Git `commit' from the " ,(symbol-name repository) " checkout.

If no commit hash provides, show a commit from hash at current point.")
       (interactive (list (read-string "Commit: " nil nil (word-at-point))))
       (let ((default-directory ,directory)
             (commit (funcall ,commit-function url)))
         (magit-show-commit commit)))))

(wi-define-magit-status-repo guix (expand-file-name "~/src/guix"))

(with-eval-after-load 'forge
  (setq forge-alist
        (append '(("gitlab.intr" "gitlab.intr/api/v4" "gitlab.intr"
                   forge-gitlab-repository)
                  ("gitlab.wugi.info" "gitlab.wugi.info/api/v4" "gitlab.wugi.info"
                   forge-gitlab-repository))
                forge-alist)))

(defvar wi-emacs-git-directory (expand-file-name "~/src/emacs")
  "Directory containing Emacs Git repository.")
(define-wi-browse-url-git-commit emacs
  wi-emacs-git-directory
  (lambda (url) (car (last (split-string url "=")))))

(defvar wi-guix-git-directory (expand-file-name "~/src/guix")
  "Directory containing Guix Git repository.")
(define-wi-browse-url-git-commit guix
  wi-guix-git-directory
  (lambda (url) (car (last (split-string url "=")))))
