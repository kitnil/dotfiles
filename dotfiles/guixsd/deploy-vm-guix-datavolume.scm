(list (machine
       (operating-system (load "vm-guix-datavolume.scm"))
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       (host-name "")
                       (system "x86_64-linux")
                       (user "user")
                       (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILqeWiJthvR8WevDO6Cc9swwB4rOjjaBXPcEF5CbKJK2 root@(none)")
                       (identity "/home/oleg/.ssh/id_rsa")))))
