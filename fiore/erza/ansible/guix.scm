(use-modules (erza ansible))

(ansible-playbook
 (hosts (list (ansible-host (name "fiore"))))
 (tasks (list
         (ansible-task
          (name "Clone Guix Git repository")
          (module "git")
          (parameters `(("repo" . "https://cgit.duckdns.org/guix")
                        ("dest" . "~/src/guix"))))
         (ansible-task
          (name "Clone guix-wigust Git repository")
          (module "git")
          (parameters `(("repo" . "https://cgit.duckdns.org/guix-wigust")
                        ("dest" . "~/src/guix-wigust"))))
         (ansible-task
          (name "Clone guix-linux-nonfree Git repository")
          (module "git")
          (parameters `(("repo" . "https://cgit.duckdns.org/guix-linux-nonfree")
                        ("dest" . "~/src/guix-linux-nonfree"))))
         (ansible-task
          (name "Compile Guix")
          (module
           `("shell" .
             ,(string-join
               '("env" "GUIX_PACKAGE_PATH=" "guix" "environment" "--pure" "guix"
                 "--ad-hoc" "help2man" "guile-sqlite3" "--" "make" "-j" "4"))))
          (parameters `("args" (("chdir" . "~/src/guix")))))
         (ansible-task
          (name "Clean Guix compile directory")
          (module
           `("shell" . ,(string-join '("git" "checkout" "--" "doc" "po")))))
         (ansible-task
          (name "Pull latest Guix")
          (module
           `("shell" .
             ,(string-join '("env" "GUIX_PACKAGE_PATH=" "guix" "pull"))))))))

