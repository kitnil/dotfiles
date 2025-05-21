(list (machine
       (operating-system (load "vm1.wugi.info.scm"))
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       (host-name "78.108.82.44")
                       (system "x86_64-linux")
                       (user "oleg")
                       (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ/LXhRvCkY5f3ZlawDmtnoqtbd9MaNdNIGLMXevbPgR root@(none)")
                       (identity "/home/oleg/.ssh/id_rsa"))))
      (machine
       (operating-system (load "vm2.wugi.info.scm"))
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       (host-name "78.108.92.69")
                       (system "x86_64-linux")
                       (user "oleg")
                       (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICxmYxMhEv54Bou6xpSv67i+OGTwMI5yxySo1cIjeptZ root@(none)")
                       (identity "/home/oleg/.ssh/id_rsa")))))
