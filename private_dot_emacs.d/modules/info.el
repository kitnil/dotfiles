(defun info-colors-fontify-reference-items ()
  "Fontify reference items in an `info' node."
  (goto-char (point-min))
  (while (re-search-forward
          (concat "^ --? \
\\("
                  (mapconcat 'identity '("Command"
                                         "Constant"
                                         "C Function"
                                         "Function"
                                         "Scheme Procedure"
                                         "Scheme Syntax"
                                         "Scheme System"
                                         "Scheme Variable"
                                         "Monadic Procedure"
                                         "Macro"
                                         "Special Form"
                                         "Syntax"
                                         "Syntax class"
                                         "User Option"
                                         "Variable")
                             "\\|")
          "\\):\
 *\\(\\S-+\\)\
\\(\\( .*\\)?\\([\n] \\{8\\}.*\\)*\\)")
          nil t)
    (let ((sym (intern (match-string 1))))
      (put-text-property (match-beginning 1) (match-end 1)
                         'font-lock-face 'info-colors-ref-item-type)
      (put-text-property
       (match-beginning 2) (match-end 2)
       'font-lock-face (case sym
                         ('Constant      'info-colors-ref-item-constant)
                         ('Variable      'info-colors-ref-item-variable)
                         ('User\ Option  'info-colors-ref-item-user-option)
                         ('Special\ Form 'info-colors-ref-item-special-form)
                         ('Macro         'info-colors-ref-item-macro)
                         ('Monadic\ Procedure 'info-colors-ref-item-function)
                         ('Scheme\ Procedure 'info-colors-ref-item-syntax-class)
                         ('Scheme\ Syntax 'info-colors-ref-item-syntax-class)
                         ('Function      'info-colors-ref-item-function)
                         ('Command       'info-colors-ref-item-command)
                         ('Syntax\ class 'info-colors-ref-item-syntax-class)))
      (when (match-beginning 3)
        (put-text-property (match-beginning 3) (match-end 3)
                           'font-lock-face 'info-colors-ref-item-other)))))

;; Origin <https://github.com/alezost/guix.el/pull/9#issuecomment-340556583>.
(with-eval-after-load 'info
  (info-initialize)
  (setq Info-directory-list
        (append (wi-expand-file-names
                 '("~/src/guix/doc"
                   "~/.guix-profile.d/gdb/share/info"
                   "~/.guix-profile.d/autotools/share/info"))
                Info-directory-list)))
;;
;; Alternative: <https://lists.gnu.org/archive/html/help-guix/2017-03/msg00140.html>,
;; see <.bashrc>.


(add-hook 'Info-selection-hook 'info-colors-fontify-node)
