(define-module (fiore deploy)
  #:use-module (fiore magnolia)
  #:use-module (gnu system)
  #:use-module (ansible)
  #:use-module (srfi srfi-26))

(define %home-directory
  (getenv "HOME"))

(define %playbooks-directory
  (string-append (getenv "HOME") "/src/ansible-wigust-playbooks"))

(define playbook-file
  (cut string-append %playbooks-directory "/" <> ".yml"))

(define* (ansible-push #:key remote remote-file playbooks)
  (for-each (lambda (playbook)
              (system* "scp" playbook (string-append remote ":" remote-file)))
            playbooks)
  (format #t "Run `ssh ansible -- ansible-playbook ~a'"
          (string-join
           (map
            (lambda (playbook)
              (string-drop playbook
                           (string-length (string-append %home-directory "/"))))
            playbooks))))


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
    (parameters `("args" (("chdir" . "~/src/guix")))))))

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
                #:remote-file
                (string-append (getenv "HOME")
                               "/src/ansible-wigust-playbooks/guix.yml")
                #:playbooks (list (ansible-playbook-file
                                   (ansible-playbook
                                    (hosts '("all"))
                                    (tasks %ansible-guix-tasks))
                                   (playbook-file "guix"))
                                  (ansible-playbook-file
                                   (ansible-playbook
                                    (hosts '("all"))
                                    (tasks %ansible-dotfiles-tasks))
                                   (playbook-file "guix")))))
