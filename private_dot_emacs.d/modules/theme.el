(setq redshift-temp-increment 100)

(setq default-frame-alist
      (append (list ;; '(width  . 73)
                    ;; '(height . 41)
                    ;; '(vertical-scroll-bars . nil)
                    '(internal-border-width . 12))
              default-frame-alist))

(defun wi-manoj-dark ()
  (interactive)
  (load-theme 'manoj-dark)
  (custom-theme-set-faces
   'manoj-dark
   '(which-func ((t (:foreground "deep sky blue"))))
   '(font-lock-function-name-face ((t (:foreground "mediumspringgreen" :weight bold :height 1.0))))
   '(diff-refine-added ((t (:inherit diff-refine-change :background "#22aa22" :foreground "aquamarine1"))))
   '(diff-refine-removed ((t (:inherit diff-refine-change :background "#aa2222" :foreground "plum1"))))
   '(which-key-command-description-face ((t (:inherit font-lock-function-name-face :height 1.0))))
   '(fringe ((t (:background "black" :foreground "Wheat"))))
   '(header-line
     ((t (:background "black" :foreground "grey90" :height 0.9))))
   ;; '(scroll-bar ((t (:background "black" :foreground "WhiteSmoke"))))
   ;; '(mode-line ((t (:background "WhiteSmoke" :foreground "black"))))
   ;; '(mode-line-inactive ((t (:background "black" :box nil))))
   ;; '(mode-line-buffer-id ((t (:background "grey15" :foreground "red"))))
   '(mode-line ((t (:background "gray5" :foreground "gray60" :inverse-video nil :box (:line-width 1 :color "black") :height 1.0))))
   '(mode-line-inactive ((t (:background "#000000" :foreground "grey20" :inverse-video nil :box (:line-width 1 :color "black") :weight light :height 1.0))))
   '(mode-line-buffer-id ((t (:background "black" :foreground "red"))))
   '(elfeed-search-title-face ((t (:foreground "dim gray"))))
   '(elfeed-search-unread-title-face ((t (:foreground "white"))))
   '(completions-common-part ((t (:width normal :weight normal
                                :slant normal :foreground "gold1"
				:background "black"))))
   '(highlight-stages-level-1-face ((t (:background "dark slate gray"))))
   '(highlight-stages-level-2-face ((t (:background "dark olive green"))))
   '(highlight-stages-level-3-face ((t (:background "sea green"))))
   '(highlight-stages-negative-level-face ((t (:background "dark cyan"))))
   '(rainbow-delimiters-depth-1-face ((t (:foreground "white"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "white"))))
   '(next-error ((t (:background "steel blue"))))
   '(region ((t (:background "steel blue"))))
   '(link ((t (:foreground "deep sky blue" :underline t))))
   '(gnus-summary-high-unread ((t (:foreground "#ffffff" :weight normal))))
   '(gnus-summary-normal-unread ((t (:foreground "white smoke")))))
  (setq sml/theme 'dark)
  (sml/setup))

(defvar current-dark-theme-p (string-equal (getenv "GTK_THEME") "Adwaita:dark")
  "If non-nil use dark theme otherwise light theme.")

(unless current-dark-theme-p (menu-bar-mode -1))

(defun wi-toggle-theme ()
  "Toggle between dark and light themes."
  (interactive)
  (if current-dark-theme-p
      (progn (mapc (lambda (theme)
                     (disable-theme theme))
                   '(manoj-dark smart-mode-line-dark))
             ;; (enable-theme 'smart-mode-line-light)
             (setq terminal-here-color 'light)
             (setq current-dark-theme-p nil))
    (progn ;; (disable-theme 'smart-mode-line-light)
           (wi-manoj-dark)
           (setq terminal-here-color 'dark)
           (setq current-dark-theme-p t))))

(setq terminal-here-color 'light)

(set-scroll-bar-mode 'right)
(scroll-bar-mode -1)
(blink-cursor-mode)

(setq hl-sexp-background-color "darkseagreen2")


;;;
;;; Smart mode-line
;;;

;; (setq sml/no-confirm-load-theme t)
;; (when (daemonp) (setq sml/theme 'light))
;; (sml/setup)

;; (custom-theme-set-faces
;;  'smart-mode-line-light
;;  '(mode-line-inactive ((t :foreground "grey20" :background "#ffffff" :inverse-video nil))))

