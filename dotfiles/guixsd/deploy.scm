(list (machine
       (operating-system (load "vm1.wugi.info.scm"))
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       (host-name "vm1.wugi.info")
                       (system "x86_64-linux")
                       (user "oleg")
                       (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICnqb74jWKmirhfbXKrt047ECzaSiCsZ4j3mYpTSbYEa root@gnu")
                       (identity "/home/oleg/.ssh/id_rsa"))))
      (machine
       (operating-system (load "vm2.wugi.info.scm"))
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       (host-name "vm2.wugi.info")
                       (system "x86_64-linux")
                       (user "oleg")
                       (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK4vfgz+tUUJFROCgTg+mRtHnr4yx1FeItXaAzmpP4lE root@(none)")
                       (identity "/home/oleg/.ssh/id_rsa"))))
      (machine
       (operating-system (load "vm3.wugi.info.scm"))
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       (host-name "vm3.wugi.info")
                       (system "x86_64-linux")
                       (user "oleg")
                       (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAc6oVzRE16IuSJUmlJWtS1QHRFxuKm9ex1gqYsRPE/z root@(none)")
                       (identity "/home/oleg/.ssh/id_rsa"))))
      (machine
       (operating-system (load "vm4.wugi.info.scm"))
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       (host-name "78.108.82.44")
                       (system "x86_64-linux")
                       (user "oleg")
                       (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ/LXhRvCkY5f3ZlawDmtnoqtbd9MaNdNIGLMXevbPgR root@(none)")
                       (identity "/home/oleg/.ssh/id_rsa"))))
      (machine
       (operating-system (load "vm5.wugi.info.scm"))
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       (host-name "178.250.247.231")
                       (system "x86_64-linux")
                       (user "oleg")
                       (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJzqatAyN7HlMxif4hhZoClNqQAmJpa7nA6y4TZ2ae3M root@(none)")
                       (identity "/home/oleg/.ssh/id_rsa"))))
      (machine
       (operating-system (load "ws1.wugi.info.scm"))
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       (host-name "ws1.wugi.info")
                       (system "x86_64-linux")
                       (user "oleg")
                       (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIA66f0IXG9Od7RImafwGjXP/RfAXssiOIwiAilC3KG4H root@workstation")
                       (identity "/home/oleg/.ssh/id_rsa")))))
