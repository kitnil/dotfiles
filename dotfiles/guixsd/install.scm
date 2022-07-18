(use-modules (gnu system)
             (services autossh))

(define (ssh-private-key)
  (and=> (getenv "HOME")
         (lambda (home)
           (or (and (file-exists? (string-append home "/.ssh/id_rsa.1"))
                    (string-append home "/.ssh/id_rsa.1"))
               (and (file-exists? (string-append home "/.ssh/id_rsa"))
                    (string-append home "/.ssh/id_rsa"))
               #f))))

(let ((%install-operating-system (load "/home/oleg/src/git.savannah.gnu.org/git/guix/gnu/system/install.scm")))
  (operating-system
    (inherit %install-operating-system)
    (services (append (list
                       (service autossh-service-type
                                (autossh-configuration
                                 (autossh-client-config
                                  (autossh-client-configuration
                                   (hosts (list (autossh-client-host-configuration
                                                 (host "back.wugi.info")
                                                 (identity-file "/etc/autossh/id_rsa")
                                                 (strict-host-key-checking? #f)
                                                 (user "vm1-ssh-tunnel")
                                                 (user-known-hosts-file "/dev/null")
                                                 (extra-options
                                                  "
RemoteForward 0.0.0.0:17022 127.0.0.1:22
RemoteForward 0.0.0.0:17050 127.0.0.1:10050
Compression yes
ExitOnForwardFailure yes
ServerAliveInterval 30
ServerAliveCountMax 3"))))))
                                 (host "back.wugi.info"))))
                      (operating-system-services %install-operating-system)))))
