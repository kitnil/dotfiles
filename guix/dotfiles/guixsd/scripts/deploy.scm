(map (lambda (system)
       (machine
        (operating-system (load (string-append system ".scm")))
        (environment managed-host-environment-type)
        (configuration (machine-ssh-configuration
                        (host-name system)
                        (system "x86_64-linux")
                        (user "oleg")
                        (identity "/home/oleg/.ssh/id_rsa")))))
     '("spb" "workstation"))
