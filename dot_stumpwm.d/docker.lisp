(in-package :stumpwm)

(defcommand docker-debian () ()
  (term-shell-command "docker run --rm -it debian:10" :scrollbar t :title "docker-debian"))

;; Lem is the editor/IDE well-tuned for Common Lisp.
;; https://github.com/cxxxr/lem
(defcommand docker-lem () ()
  (term-shell-command "docker run --rm -ti 40ants/lem:latest" :title "docker-lem"))
