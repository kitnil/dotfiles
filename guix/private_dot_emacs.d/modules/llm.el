(with-eval-after-load 'gptel
  (setq gptel-backend (gptel-make-openai "lmstudio"
                        :stream t
                        :protocol "https"
                        :host "lm-studio.home.wugi.info"
                        :models '(lmstudio)))
  (when (macrop #'bind-key)
    (bind-key "C-c g l <return>" #'gptel-send)
    (bind-key "C-c g l b" #'gptel)))
