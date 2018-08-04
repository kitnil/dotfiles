(define-module (fiore deploy)
  #:use-module (fiore magnolia)
  #:use-module (gnu system)
  #:use-module (ansible)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match))

(define %home-directory
  (getenv "HOME"))

(define %playbooks-directory
  (string-append (getenv "HOME") "/src/ansible-wigust-playbooks"))

(define playbook-file
  (cut string-append %playbooks-directory "/" <> ".yml"))

(define* (ansible-push #:key remote playbooks)
  (for-each (match-lambda
              ((playbook . remote-file)
               (system* "scp" playbook (string-append remote ":" remote-file))
               (format #t "Run `ssh ansible -- ansible-playbook ~a'.\n"
                       (string-drop playbook
                                    (string-length (string-append %home-directory "/"))))))
            playbooks))


;;;
;;; Tasks
;;;

(define %ansible-guix-tasks
  (list
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
     `("shell" . ,(string-join '("git" "checkout" "--" "doc" "po"))))
    (parameters '()))
   (ansible-task
    (name "Pull latest Guix")
    (module
     `("shell" .
       ,(string-join '("env" "GUIX_PACKAGE_PATH=" "guix" "pull"))))
    (parameters '()))))

(define %ansible-dotfiles-tasks
  (list
   (ansible-task
    (name "Clone dotfiles repository")
    (module "git")
    (parameters `(("repo" . "https://cgit.duckdns.org/dotfiles")
                  ("dest" . "~/dotfiles"))))))


;;;
;;; Push
;;;

(define (fiore-push)
  (ansible-push #:remote "ansible"
                #:playbooks
                (list (cons (ansible-playbook-file
                             (ansible-playbook
                              (hosts '("all"))
                              (tasks %ansible-guix-tasks))
                             (playbook-file "guix"))
                            (string-append %home-directory
                                           "/src/ansible-wigust-playbooks/guix.yml"))
                      (cons (ansible-playbook-file
                             (ansible-playbook
                              (hosts '("all"))
                              (tasks %ansible-dotfiles-tasks))
                             (playbook-file "dotfiles"))
                            (string-append %home-directory
                                           "/src/ansible-wigust-playbooks/dotfiles.yml")))))
