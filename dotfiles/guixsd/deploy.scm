(list (machine
       (operating-system (load "guix.vm.wugi.info.scm"))
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       (host-name "guix.vm.wugi.info")
                       (system "x86_64-linux")
                       (user "oleg")
                       (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICnqb74jWKmirhfbXKrt047ECzaSiCsZ4j3mYpTSbYEa root@gnu")
                       (identity "/home/oleg/.ssh/id_rsa"))))
      (machine
       (operating-system (load "vm32593.scm"))
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       (host-name "vm2.wugi.info")
                       (system "x86_64-linux")
                       (user "oleg")
                       (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK4vfgz+tUUJFROCgTg+mRtHnr4yx1FeItXaAzmpP4lE root@(none)")
                       (identity "/home/oleg/.ssh/id_rsa"))))
      (machine
       (operating-system (load "workstation-guixsd.scm"))
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       (host-name "172.16.100.60")
                       (system "x86_64-linux")
                       (user "oleg")
                       (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIA66f0IXG9Od7RImafwGjXP/RfAXssiOIwiAilC3KG4H root@workstation")
                       (identity "/home/oleg/.ssh/id_rsa")))))
