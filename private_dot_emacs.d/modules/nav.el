(defun wi-set-current-frame-80-40 ()
  "Set current frame to 80 pixels width and 40 pixels height."
  (interactive)
  (set-frame-size (selected-frame) 80 40))

(defun wi-set-current-frame-80-24 ()
  "Set current frame to 80 pixels width and 24 pixels height."
  (interactive)
  (set-frame-size (selected-frame) 80 24))

;; Origin <https://github.com/Wilfred/.emacs.d/blob/gh-pages/init.org>.
(setq scroll-preserve-screen-position 'always) ; Preserve scroll pos.
(setq dumb-jump-max-find-time 4)

;; Origin <https://github.com/Wilfred/.emacs.d/blob/gh-pages/init.org>.
(setq enable-recursive-minibuffers t) ; Enable recursive minibuffer.
(minibuffer-depth-indicate-mode)      ; Show recursion depth.

;; (with-eval-after-load 'tramp
;;   (recentf-mode 1))
(setq recentf-max-menu-items 500)
(setq recentf-max-saved-items nil)
(setq recentf-auto-cleanup 'never)

;; See <https://www.emacswiki.org/emacs/DoWhatIMean>
(setq dired-dwim-target t)

(default-text-scale-mode)
(global-git-gutter-mode)

(add-hook 'diff-mode-hook (lambda () (setq-local truncate-lines t)))

(setq dumb-jump-selector 'ivy)
(setq dumb-jump-force-searcher 'ag)

(autoload 'crux-transpose-windows "crux" nil t)
(autoload 'crux-open-with "crux" nil t)

(projectile-global-mode)
(define-key projectile-mode-map (kbd "C-c g p") 'projectile-command-map)

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

(defun wi-switch-to-scratch-elisp ()
  "Switch to *scratch* buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(beginend-global-mode)

(save-place-mode)            ; Remember position in files
(setq mouse-yank-at-point t) ; Ignore mouse position on paste

;; Toggle prettify symbols mode on
(global-prettify-symbols-mode)

 ; Unprettify symbol after the cursor
(setq prettify-symbols-unprettify-at-point 'right-edge)

;; Undo and redo operations on windows and buffers
(winner-mode 1)
(windmove-default-keybindings)

;; Display key bindings help window (after some delay)
(which-key-mode)

;; Don't use ido
(setq projectile-completion-system 'ivy)

(add-to-list 'projectile-project-root-files "environment-variables")

;; Toggle show-paren-mode on
(show-paren-mode)

(add-hook 'prog-mode-hook 'hl-todo-mode)
;; (add-hook 'prog-mode-hook 'which-function-mode)



