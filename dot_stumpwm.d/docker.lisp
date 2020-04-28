(in-package :stumpwm)

(defcommand docker-debian () ()
  (term-shell-command "docker run --rm -it debian:10" :scrollbar t :title "docker-debian"))
