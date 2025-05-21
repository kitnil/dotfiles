(list (machine
       (operating-system (load "minimal.tmpl"))
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       (host-name "localhost")
                       (system "x86_64-linux")
                       (user "root")
                       (port 30022)
                       (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOCrIFUeZKxASSXYoVGH8ltOXd2/N36apayOxLkXBqmm root@(none)")
                       (identity "/home/oleg/.ssh/id_rsa_vm1.wugi.info")))))
