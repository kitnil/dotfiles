(use-modules (gnu bash)
             (guix build utils)
             (guix ui)
             (ice-9 format)
             (srfi srfi-41))

(define-bash-function (hello)
  (display "hello")
  (newline))

(define-bash-function (mjru-web)
  (format #t "~{~a~%~}" (stream->list (stream-range 0 10))))

(define %home
  (and=> (getenv "HOME")
         (lambda (home)
           home)))

(define-bash-function (run-vm-guix)
  (invoke "qemu-system-x86_64"
          "-smp" "cores=4,threads=1"
          "-m" "4096"
          "-enable-kvm"
          "-cpu" "host"
          "-daemonize"
          "-vnc" ":6"
          "-nic" "user,model=virtio-net-pci,hostfwd=tcp::10022-:22"
          (string-append %home "/vm/guix.qcow2"))
  (display-hint (string-join '("ssh"
                               "-o" "UserKnownHostsFile=/dev/null"
                               "-o" "StrictHostKeyChecking=no"
                               "-p" "10022"
                               "localhost")))
  (display-hint (string-join '("vnc" "client" "5906"))))
