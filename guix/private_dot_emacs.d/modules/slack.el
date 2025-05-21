(setq slack-teams nil)
(setq slack-prefer-current-team t)

;; https://github.com/yuya373/emacs-slack/issues/431
(slack-register-team
 :name "mjru"
 :default t
 :token (let ((secret (plist-get (nth 0
                                      (auth-source-search
                                       :host "slack-majordomo"
                                       :user "pyhalov"))
                                 :secret)))
          (and (functionp secret)
               (funcall secret)))
 :subscribed-channels '(monitoring git tech eng sups)
 :full-and-display-names t)

(setq slack-completing-read-function 'ivy-completing-read)
