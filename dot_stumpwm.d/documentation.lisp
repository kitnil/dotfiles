(in-package :stumpwm)

(defun zathura (file)
  (run-shell-command (format nil "zathura ~s" file)))

(defcommand doc-arcconf () ()
  (zathura (concat (getenv "HOME") "/.guix-profile/share/doc/arcconf/arcconf.pdf")))

(defcommand doc-jenkins-casc () ()
  (zathura (concat (getenv "HOME") "/.guix-profile/share/doc/devops-world-jenkins-casc/devops-world-jenkins-casc.pdf")))

(defcommand doc-bash () ()
  (zathura (concat (getenv "HOME") "/.guix-profile/share/doc/concise-gnu-bash/concise-gnu-bash.pdf")))

(defcommand doc-perf () ()
  (zathura (concat (getenv "HOME") "/.guix-profile/share/doc/linux-perf-tools/linux-perf-tools.pdf")))
