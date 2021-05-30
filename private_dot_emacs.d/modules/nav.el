(defun wi-set-current-frame-80-40 ()
  "Set current frame to 80 pixels width and 40 pixels height."
  (interactive)
  (set-frame-size (selected-frame) 80 40))

(defun wi-set-current-frame-80-24 ()
  "Set current frame to 80 pixels width and 24 pixels height."
  (interactive)
  (set-frame-size (selected-frame) 80 24))

;; Origin https://emacsredux.com/blog/2020/07/18/automatically-kill-running-processes-on-exit/
(setq confirm-kill-processes nil)

;; Open buffer in vertical split by default
(setq split-height-threshold nil)
(setq split-width-threshold 160)

;; Origin <https://github.com/Wilfred/.emacs.d/blob/gh-pages/init.org>.
(setq scroll-preserve-screen-position 'always) ; Preserve scroll pos.
(setq dumb-jump-max-find-time 4)

;; Origin <https://github.com/Wilfred/.emacs.d/blob/gh-pages/init.org>.
(setq enable-recursive-minibuffers t) ; Enable recursive minibuffer.

;; (with-eval-after-load 'tramp
;;   (recentf-mode 1))
(setq recentf-max-menu-items 30)
(setq recentf-max-saved-items nil)
(setq recentf-auto-cleanup 'never)

;; See <https://www.emacswiki.org/emacs/DoWhatIMean>
(setq dired-dwim-target t)

(add-hook 'diff-mode-hook (lambda () (setq-local truncate-lines t)))

(setq dumb-jump-selector 'ivy)
(setq dumb-jump-force-searcher 'ag)

(autoload 'crux-transpose-windows "crux" nil t)
(autoload 'crux-open-with "crux" nil t)

;; (global-undo-tree-mode)
;; (setq undo-tree-auto-save-history t)
;; (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;; Default from Emacs 26
;; See <http://git.savannah.gnu.org/cgit/emacs.git/commit/etc/NEWS?id=72ee93d68daea00e2ee69417afd4e31b3145a9fa>
(setq print-quoted t)

;; Enable functions
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

(setq-default fill-column 78)

(defun wi-switch-to-scratch-elisp ()
  "Switch to *scratch* buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(setq mouse-yank-at-point t) ; Ignore mouse position on paste
(setq mouse-autoselect-window t) ; Sloppy mouse

 ; Unprettify symbol after the cursor
(setq prettify-symbols-unprettify-at-point 'right-edge)

(setq whitespace-style
      '(face tabs
             spaces
             trailing
             ;; lines
             space-before-tab
             newline
             indentation
             empty
             space-after-tab
             space-mark
             tab-mark
             ;; newline-mark
             ))

(add-hook 'prog-mode-hook 'hl-todo-mode)
;; (add-hook 'prog-mode-hook 'which-function-mode)
(add-hook 'prog-mode-hook 'symbol-overlay-mode)

(setq wi-groups-direcotory "~/majordomo")
(setq wi-projects-directories '("~/src" "~/archive/src"))

(defun wi-find-file-readlink ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (kill-buffer (current-buffer))
    (find-file
     (string-trim
      (shell-command-to-string
       (format "readlink -f %s" file-name))))))

(add-hook 'find-file-hook
          '(lambda ()
             (when (string-match (rx (and any ".guix-profile")) (buffer-file-name))
               (wi-find-file-readlink))))

(defalias 'center-mode 'olivetti-mode)

;; Show recursion depth.
(progn
  (minibuffer-depth-indicate-mode)

  (global-git-gutter-mode)

  (default-text-scale-mode)

  (projectile-global-mode)

  ;; Don't use ido
  (setq projectile-completion-system 'ivy)

  (add-to-list 'projectile-project-root-files "environment-variables")

  (setq projectile-switch-project-action 'projectile-dired)
  (define-key projectile-mode-map (kbd "C-c g p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c g p h") 'helm-projectile)

  (beginend-global-mode)
  (save-place-mode)            ; Remember position in files

  ;; Toggle prettify symbols mode on
  (global-prettify-symbols-mode)

  ;; Undo and redo operations on windows and buffers
  (winner-mode 1)
  (windmove-default-keybindings)

  ;; Display key bindings help window (after some delay)
  (which-key-mode)

  ;; Toggle show-paren-mode on
  (show-paren-mode))

;; Emacs redraw issue in X.org on VMWare and VirtualBox | fujii.github.io
;; <https://fujii.github.io/2016/09/06/emacs-redisplay-issue-on-vmware/>
(when (eq window-system 'x)
  (add-hook 'window-scroll-functions
            (lambda (&rest x)
              (run-with-idle-timer 0.5 nil 'redraw-display))))
