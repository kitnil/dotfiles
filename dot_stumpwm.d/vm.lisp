
(defcommand qemu-debian () ()
  "Run GNOME Debian in QEMU."
  (run-shell-command (concat "exec " (getenv "HOME") "/bin/debian.sh")))

(defcommand debian-unstable () ()
  (run-shell-command
   (join (list "qemu-system-x86_64" "-daemonize" "-smp" "cores=4,threads=1"
               "-m" "4096" "-enable-kvm" "-cpu" "host" "-daemonize"
               "-vnc" ":5" "-hda" "~/vm/debian-unstable.qcow2"
               "-cdrom" "~/Downloads/debian-testing-amd64-netinst.iso"))))

(defcommand epson () ()
  (term-shell-command "sudo qemu-epson.sh"))

(defcommand epson-no-graphic () ()
  (term-shell-command "sudo qemu-epson.sh -display none"))
