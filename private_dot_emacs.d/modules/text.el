(setq rotate-text-symbols
      '(("define" "define*" "define-public")
        ("private" "protected" "public")
        ("android-ndk-build-system" "ant-build-system"
         "asdf-build-system" "cargo-build-system" "clojure-build-system"
         "cmake-build-system" "dub-build-system" "dune-build-system"
         "emacs-build-system" "font-build-system"
         "glib-or-gtk-build-system" "gnu-build-system" "go-build-system"
         "guile-build-system" "haskell-build-system"
         "linux-module-build-system" "meson-build-system"
         "minify-build-system" "ocaml-build-system" "perl-build-system"
         "python-build-system" "rakudo-build-system" "r-build-system"
         "ruby-build-system" "scons-build-system" "texlive-build-system"
         "trivial-build-system" "waf-build-system")))

(setq highlight-indent-guides-method 'character)

;; TODO: Implement wi-soften-hardlines
;; Origin <https://lists.gnu.org/archive/html/emacs-devel/2017-12/msg00518.html>.
;; See also <https://github.com/legoscia/messages-are-flowing>.
;; (defun wi-soften-hardlines ()
;;     (interactive)
;;     (save-excursion
;;       (goto-char (point-min))
;;       (mail-text)
;;       (while (search-forward hard-newline nil t)
;;         (replace-match "\n"))))

(defun wi-sort-sexps (reverse beg end)
  "Sort sexps in the Region."
  (interactive "*P\nr")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (let ((nextrecfun (lambda () (skip-syntax-forward "-.>")))
          (endrecfun  #'forward-sexp))
      (sort-subr reverse nextrecfun endrecfun))))

(defun wi-find-stumpwm-init-file ()
  "Edit the `stumpwm-init-file', in another window."
  (interactive)
  (find-file-other-window
   (expand-file-name "~/.stumpwm.d/init.lisp")))

(defun wi-copy-file-name ()
  "Return current buffer file name."
  (interactive)
  (kill-new (buffer-file-name)))

(defun wi-copy-project-file-name ()
  "Return current buffer file name in current project."
  (interactive)
  (kill-new (file-relative-name (buffer-file-name)
                                (funcall (cl-find-if 'fboundp
                                                     '(projectile-project-root
                                                       vc-root-dir))))))

;; Origin <https://emacs.stackexchange.com/a/2473>.
(defun wi-dabbrev-expand ()
  "Insert space and call `dabbrev-expand'."
  (interactive)
  (execute-kbd-macro (kbd "SPC"))
  (call-interactively #'dabbrev-expand))

(defun wi-dabbrev-expand-until-period ()
  "Call `wi-dabbrev-expand' until period before cursor."
  (interactive)
  (unless (string-equal (char-to-string (char-before)) ".")
    (wi-dabbrev-expand)
    (wi-dabbrev-expand-until-period)))

(defun wi-mark-paragraph+sort-lines ()
  "Invoke `mark-paragraph' and `sort-lines'."
  (interactive)
  (mark-paragraph)
  (sort-lines nil (region-beginning) (region-end)))

;; TODO: Implement wi-define-insert
(defmacro wi-define-insert (name-text-list)
  `(mapc (lambda (name-text)
           (let ((name (first name-text))
                 (text (second name-text)))
             (defun ,(intern (concat "wi-insert-" (symbol-name name)))
                 nil
               (interactive)
               (insert text))))
         ,name-text-list))

(defun close-all-parentheses ()
  "Close all parentheses."
  (interactive "*")
  (let ((closing nil))
    (save-excursion
      (while (condition-case nil
         (progn
           (backward-up-list)
           (let ((syntax (syntax-after (point))))
             (case (car syntax)
               ((4) (setq closing (cons (cdr syntax) closing)))
               ((7 8) (setq closing (cons (char-after (point)) closing)))))
           t)
           ((scan-error) nil))))
    (apply #'insert (nreverse closing))))


(defun wi-copy-buffer (buffer)
  "Copy BUFFER to kill ring and save in the GUI’s clipboard."
  (with-current-buffer (get-buffer buffer)
    (save-excursion
      (clipboard-kill-ring-save (point-min) (point-max)))))

(defun wi-copy-current-buffer ()
  "Copy current buffer to kill ring and save in the GUI’s clipboard."
  (interactive)
  (wi-copy-buffer (current-buffer)))

(defun wi-replace-with-brackets-ellipsis ()
  "Replace region with \"[…]\"."
  (interactive)
  (kill-region (region-beginning) (region-end))
  (insert "[…]")
  (newline 2))

(defun wi-ttn-hs-hide-level-1 ()
  (hs-hide-level 1)
  (forward-sexp 1))

(setq hs-hide-all-non-comment-function 'wi-ttn-hs-hide-level-1)

;; Deletes up to the provided character
;; Doesn’t delete the provided character
;; Starts the point from before the character rather than after
;;
;; Source: https://www.emacswiki.org/emacs/ZapUpToChar
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)


;;;
;;; Anywhere
;;;

(setq anywhere-kill-buffer nil)
(setq anywhere-major-mode 'text-mode)
(add-hook 'anywhere-mode-hook '(lambda () (activate-input-method "russian-computer")))
(add-hooks '(((anywhere-mode-hook atomic-chrome-edit-mode-hook)
              . (lambda ()
                  (set (make-local-variable 'prettify-symbols-alist)
                       wi-scheme--prettify-symbols-alist)))
             ;; ((anywhere-mode-hook atomic-chrome-edit-mode-hook)
             ;;  . (lambda () (set-input-method "russian-computer")))
             ((anywhere-mode-hook atomic-chrome-edit-mode-hook)
              . (lambda () (ispell-change-dictionary "ru")))
             ((anywhere-mode-hook atomic-chrome-edit-mode-hook)
              . flyspell-mode)
             ((anywhere-mode-hook atomic-chrome-edit-mode-hook forge-post-mode-hook)
              . abbrev-mode)
             ((anywhere-mode-hook atomic-chrome-edit-mode-hook)
              . yas-minor-mode)
             ((anywhere-mode-hook atomic-chrome-edit-mode-hook)
              . (lambda ()
                  (setq-local company-idle-delay 0.1)
                  (setq-local company-minimum-prefix-length 2)))
             ((anywhere-mode-hook atomic-chrome-edit-mode-hook)
              . visual-line-mode)))

(with-eval-after-load 'anywhere-mode
  (let ((map anywhere-mode-map))
    (define-key map (kbd "C-c '") 'anywhere-exit)
    (define-key map (kbd "C-c i") 'ispell-buffer)
    (define-key map (kbd "C-c v") 'ivy-yasnippet)))


;;;
;;; Smartparens
;;;

;; Structured editing
(with-eval-after-load 'smartparens
  (require 'smartparens-config nil t)
  (setq sp-highlight-pair-overlay nil)
  (sp-use-smartparens-bindings)

  ;; Origin <https://github.com/Fuco1/smartparens/blob/master/docs/pair-management.rst>.
  (sp-pair "“" "”")
  (sp-pair "‘" "’")
  (sp-local-pair 'text-mode "<" ">"))


;;;
;;; Translate
;;;

;; Google translate with translate-shell program
(require 'google-translate-mode nil t)
(with-eval-after-load 'google-translate-mode
  (setq trans-target "ru"))


