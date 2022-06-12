(use-modules (guix build utils)
             (guix gexp)
             (guix modules)
             (guix monads)
             (guix packages)
             (guix profiles)
             (guix records)
             (guix store)
             (ice-9 match)
             (json builder)
             (srfi srfi-1)
             (gnu)
             (gnu machine))

(define %dotfiles-directory
  (and=> (getenv "HOME")
         (lambda (home)
           (string-append home "/.local/share/chezmoi"))))

(define %ansible-playbook
  (list->vector
   `((("tasks"
       .
       #((("name" . "Bootstrap a host without python or git installed")
          ("raw" . "([[ -e ~/.guix-profile/bin/python3 ]] && [[ -e ~/.guix-profile/bin/git ]]) || guix install python git"))
         (("name" . "Clone dotfiles")
          ("git"
           ("repo" . "https://github.com/kitnil/dotfiles")
           ("dest" . ,%dotfiles-directory)))
         (("name" . "Pull Guix")
          ("guix_pull"
           ("channels" . ,(string-append %dotfiles-directory "/dotfiles/channels-current.scm"))))
         (("name" . "Apply Guix manifest")
          ("guix_package"
           ("profile" . ,%user-profile-directory)
           ("manifest" . ,(string-append %dotfiles-directory "/dotfiles/manifests/{{ ansible_nodename }}.scm"))
           ("load_path" . ,(string-append %dotfiles-directory "/dotfiles/guixsd/modules"))))))
      ("hosts" . #("guix_vm" "guix_work")))
     (("tasks"
       .
       #((("name" . "Pull Guix")
          ("guix_pull"
           ("channels" . ,(string-append %dotfiles-directory "/dotfiles/channels-current.scm"))))))
      ("hosts" . #("guix_vm" "guix_work"))
      ("become_flags" . "-i")
      ("become" . "yes")))))

(define (ansible-playbook playbook)
  (mlet* %store-monad ((main.json (text-file* "main.json"
                                              (with-output-to-string
                                                (lambda ()
                                                  (scm->json playbook #:pretty #t)))))
                       (program ->
                                (program-file
                                 "ansible-program"
                                 (with-imported-modules '((guix build utils))
                                   #~(begin
                                       (use-modules (guix build utils))
                                       (invoke "ansible-playbook" #$main.json))))))
    (gexp->derivation "ansible-program" #~(symlink #$program #$output))))

(run-with-store (open-connection)
  (ansible-playbook %ansible-playbook))
