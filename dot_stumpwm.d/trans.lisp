
(defcommand trans-en-ru () ()
  "Run `xterm' with `trans' in interactive mode."
  (term-shell-command "trans -I en:ru" :title "trans-en-ru"))

(defcommand trans-ru-en () ()
  "Run `xterm' with `trans' in interactive mode."
  (term-shell-command "trans -I ru:en" :title "trans-ru-en"))
