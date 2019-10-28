(and=> (getenv "HOME")
       (lambda (home)
         (let ((key-my (string-append home "/.ssh/id_rsa"))
               (key-oracle (string-append home "/.ssh/id_rsa_oracle")))
           (with-output-to-file "/tmp/config"
             (lambda ()
               (display (string-join `("\
Host br1-mr14.intr
  User root
  StrictHostKeyChecking no
  HashKnownHosts no"

                                       "\
Host sr1-dh507-508.intr
  User root
  StrictHostKeyChecking no
  HashKnownHosts no"

                                       ,(string-append "\
# [opc@instance-20190921-1242 ~]$
Host oracle
  HostName 130.61.52.156
  " (format #f "IdentityFile ~a" key-oracle) "
  User opc")

                                       "\
Host nixos
  HostName localhost
  Port 2222
  User root
  StrictHostKeyChecking no
  HashKnownHosts no
  UserKnownHostsFile /dev/null"

                                       "\
Host github.com
  User git
  IdentityFile ~/.ssh/id_rsa_github"

                                       "\
Host clover
  IdentityFile ~/.ssh/id_rsa_clover
  HostName 192.168.105.130
  User natsu"

                                       "\
Host git.sv.gnu.org
  IdentityFile ~/.ssh/id_rsa_savannah
  User wigust"

                                       "\
Host gitea.wugi.info
  IdentityFile ~/.ssh/id_rsa_gitea
  Port 222
  User git
  HostName gitea.wugi.info"

                                       "\
Host gitlab.intr
  IdentityFile ~/.ssh/id_rsa_gitlab_intr
  User git"

                                       "\
Host spb
  IdentityFile ~/.ssh/id_rsa
  Port 19022
  PreferredAuthentications publickey
  StrictHostKeyChecking no
  User oleg
  HostName localhost"

                                       "\
Host gitlab.com
  IdentityFile ~/.ssh/id_rsa_gitlab
  User wigust"

                                       ,(string-append "\
Host gitlab.wugi.info
  " (format #f "IdentityFile ~a" key-my) "
  Port 65022
  User git
  HostName localhost")

                                       "\
Host *.intr
  IdentityFile ~/.ssh/eng_key_rsa
  User eng
  ControlMaster auto
  ControlPath ~/.ssh/master-%r@%h:%p
  ControlPersist yes"

                                       ,(string-append "\
Host work
  " (format #f "IdentityFile ~a" key-my) "
  PreferredAuthentications publickey
  User user
  HostName 172.16.100.60")

                                       ,(string-append "\
Host majordomo
  " (format #f "IdentityFile ~a" key-my) "
  Port 9999
  PreferredAuthentications publickey
  User user
  HostName localhost")

                                       "\
Host notabug.org
  IdentityFile ~/.ssh/id_rsa_notabug
  User wigust"

                                       "\
Host web*s.majordomo.ru
  IdentityFile ~/.ssh/id_rsa_sup
  Port 1022
  StrictHostKeyChecking no
  User sup
  HostName %h"

                                       "\
Host router.majordomo.ru
  User root
  IdentityFile ~/.ssh/eng_key_rsa
  Port 1022
  KexAlgorithms +diffie-hellman-group1-sha1
  HostKeyAlgorithms +ssh-dss
  # Ciphers +aes256-cbc
")
                                     "\n\n")
                        ))))))
