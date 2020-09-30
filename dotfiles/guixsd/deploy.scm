(list (machine
       (operating-system (load "guix.vm.wugi.info.scm"))
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       (host-name "guix.vm.wugi.info")
                       (system "x86_64-linux")
                       (user "oleg")
                       (identity "/home/oleg/.ssh/id_rsa")))))
