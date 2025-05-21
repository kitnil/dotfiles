(list ;; (machine
      ;;  (operating-system (load "ws2.wugi.info.scm"))
      ;;  (environment managed-host-environment-type)
      ;;  (configuration (machine-ssh-configuration
      ;;                  (host-name "172.16.103.226")
      ;;                  (system "x86_64-linux")
      ;;                  (user "oleg")
      ;;                  (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPvDZSe5l2m9FPxvmtkQTYcyXgj13+O43cTe1338xVYa root@gnu")
      ;;                  (identity "/home/oleg/.ssh/id_rsa"))))
      (machine
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
