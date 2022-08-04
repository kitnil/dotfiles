(in-package :stumpwm)

(defcommand suspend () ()
  (if (y-or-n-p "Suspend the system? ")
      (run-shell-command "sudo loginctl suspend")))

(defcommand halt () ()
  (if (y-or-n-p "Halt the system? ")
      (run-shell-command "sudo halt")))

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
           (file-get-contents
            (first
             (select-from-menu
              (current-screen)
              (mapcar (lambda (path)
                        (format nil "~f" path))
                      (uiop/filesystem:directory-files (concat (getenv "HOME") "/.ssh") "*.pub"))))))))

(defcommand insert-eng-key () ()
  (window-send-string
   (format nil "mkdir ~~/.ssh; cat >> ~~/.ssh/authorized_keys <<'EOF'~%~aEOF"
           (file-get-contents (concat (getenv "HOME") "/.ssh/id_rsa_majordomo_eng.pub")))))

(defcommand xpanes-guix () ()
  (term-shell-command "xpanes -t -C 1 -c 'ssh -t {}' guixsd workstation.intr spb"
                      :color 'dark))


