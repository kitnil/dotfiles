(in-package :stumpwm)

(defcommand suspend () ()
  (run-shell-command "sudo loginctl suspend"))

(defcommand neofetch () ()
  (term-shell-command "sh -c 'neofetch; read'"))


;;;
;;; Monitoring
;;;

(defcommand glances () ()
  (term-shell-command "glances"))

(defcommand sampler () ()
  (term-shell-command "sampler -c ~/.config/sampler/config.yaml"
                      :color "dark"
                      :font '("-fa" "Monospace" "-fs" "10")))

(defcommand htop () ()
  (term-shell-command "htop"))

(defcommand top () ()
  (term-shell-command "top"))

(defcommand xpanes-top () ()
  (term-shell-command "xpanes -t -C 1 -c 'autossh -M0 -t {} -- top -d 10' localhost spb workstation.intr ci.intr kvm15.intr oracle"
                      :terminal 'st
                      :title "xpanes-top"
                      :font "Monospace:size=6"))


;;;
;;; SSH
;;;

(defcommand insert-ssh-key () ()
  (window-send-string
   (format nil "mkdir ~~/.ssh; cat >> ~~/.ssh/authorized_keys <<'EOF'~%~aEOF"
           (file-get-contents (concat (getenv "HOME") "/.ssh/id_rsa.pub")))))

(defcommand xpanes-guix () ()
  (term-shell-command "xpanes -t -C 1 -c 'ssh -t {}' guixsd workstation.intr spb"
                      :color 'dark))


