(list (machine
       (operating-system (load "guix.vm.wugi.info.scm"))
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       (host-name "guix.vm.wugi.info")
                       (system "x86_64-linux")
                       (user "oleg")
                       (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICnqb74jWKmirhfbXKrt047ECzaSiCsZ4j3mYpTSbYEa root@gnu")
                       (identity "/home/oleg/.ssh/id_rsa")))))
