;; https://endlessparentheses.com/restarting-the-compilation-buffer-in-comint-mode.html
;; https://endlessparentheses.com/better-compile-command.html
;; https://endlessparentheses.com/provide-input-to-the-compilation-buffer.html

;; Stop on the first error.
(setq compilation-scroll-output 'first-error)

;; I'm not scared of saving everything.
(setq compilation-ask-about-save nil)

;; Don't stop on info or warnings.
(setq compilation-skip-threshold 2)

(defcustom endless/compile-window-size 105
  "Width given to the non-compilation window."
  :type 'integer
  :group 'endless)

(defun endless/compile-please (comint)
  "Compile without confirmation.
With a prefix argument, use comint-mode.

Now I can actually interact with this REPL via C-c i or just
quickly get rid of it with C-d. If you run into the same
situation, you should also set the following option in your
.pryrc file."
  (interactive "P")
  ;; Do the command without a prompt.
  (save-window-excursion
    (compile (eval compile-command) (and comint t)))
  ;; Create a compile window of the desired width.
  (pop-to-buffer (get-buffer "*compilation*"))
  (enlarge-window
   (- (frame-width)
      endless/compile-window-size
      (window-width))
   'horizontal))

(defun endless/toggle-comint-compilation ()
  "Restart compilation with (or without) `comint-mode'."
  (interactive)
  (cl-callf (lambda (mode) (if (eq mode t) nil t))
      (elt compilation-arguments 1))
  (recompile))

(define-key compilation-mode-map (kbd "C-c i")
  #'endless/toggle-comint-compilation)
(define-key compilation-minor-mode-map (kbd "C-c i")
  #'endless/toggle-comint-compilation)
(define-key compilation-shell-minor-mode-map (kbd "C-c i")
  #'endless/toggle-comint-compilation)
