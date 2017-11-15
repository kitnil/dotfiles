(when (display-graphic-p)
  (load-theme 'manoj-dark)
  ;; (setq font-use-system-font t)
  ;; (blink-cursor-mode -1)
  )

(setq guix-read-package-name-function #'guix-read-package-name-at-point)

(global-set-key (kbd "C-c f f") #'ffap)
(global-set-key (kbd "C-c f p") #'guix-edit)
(global-set-key (kbd "C-c b b") #'ibuffer)
(global-set-key (kbd "C-c v s") #'magit-status)

(setq browse-url-browser-function
      `(("^ftp://.*" . browse-ftp-tramp)
        ("^https?://debbugs\\.gnu\\.org/.*" . debbugs-browse-url)
        ("^https?://w*\\.?youtube.com/watch\\?v=.*" . browse-url-mpv)
        ("^https?://w*\\.?youtube.com/.*" . browse-url-chromium)
        ("^https?://w*\\.?github.com/.*" . browse-url-chromium)
        ("." . browse-url-firefox)))

(setq guix-directory "~/src/guix")

(add-to-list 'Info-directory-list "/home/natsu/src/guile-chickadee/doc")
(add-to-list 'Info-directory-list "/home/natsu/src/guix/doc")
(add-to-list 'Info-directory-list "/home/natsu/src/stumpwm")

(setq debbugs-gnu-default-packages (list "guix" "guix-patches"))

(setq send-mail-function #'smtpmail-send-it)
(setq smtpmail-smtp-server "smtp.gmail.com")

;; (add-hook 'scheme-mode-hook #'geiser-mode)

(with-eval-after-load 'geiser-guile
  (add-to-list 'geiser-guile-load-path "/home/natsu/src/guix")
  (setq geiser-guile-binary '("guile" "--no-auto-compile")))

(defun debbugs-gnu-guix ()
  (interactive)
  (debbugs-gnu '("serious" "important" "normal") '("guix")))

(defun debbugs-gnu-guix-patches ()
  (interactive)
  (debbugs-gnu '("serious" "important" "normal") '("guix-patches")))

(defun set-current-frame-80-40 ()
  (interactive)
  (set-frame-size (selected-frame) 80 40))

(defun set-current-frame-80-24 ()
  (interactive)
  (set-frame-size (selected-frame) 80 24))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   (quote
    ((eval modify-syntax-entry 43 "'")
     (eval modify-syntax-entry 36 "'")
     (eval modify-syntax-entry 126 "'")
     (bug-reference-bug-regexp . "<https?://\\(debbugs\\|bugs\\)\\.gnu\\.org/\\([0-9]+\\)>")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
