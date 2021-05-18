(define-module (bash)
  #:use-module (guix build utils)
  #:use-module (guix ui)
  #:use-module (ice-9 format)
  #:export (run-vm-guix))

(define %home
  (and=> (getenv "HOME")
         (lambda (home)
           home)))

(define (run-vm-guix)
  (display-hint (string-join (list "qemu-system-x86_64"
                                   "-smp" "cores=4,threads=1"
                                   "-m" "4096"
                                   "-enable-kvm"
                                   "-cpu" "host"
                                   "-daemonize"
                                   "-vnc" ":6"
                                   "-nic" "user,model=virtio-net-pci,hostfwd=tcp::10022-:22"
                                   (string-append %home "/vm/guix.qcow2"))))
  (display-hint (string-join '("ssh"
                               "-o" "UserKnownHostsFile=/dev/null"
                               "-o" "StrictHostKeyChecking=no"
                               "-p" "10022"
                               "localhost")))
  (display-hint (string-join '("vnc" "client" "5906"))))
