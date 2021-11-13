(use-modules (guix build utils)
             (guix gexp)
             (guix modules)
             (guix monads)
             (guix packages)
             (guix records)
             (guix store)
             (ice-9 match)
             (json builder)
             (srfi srfi-1)
             (gnu packages admin))

(define %ansible-playbook
  #((("tasks"
      .
      #((("raw" . "([[ -e ~/.guix-profile/bin/python3 ]] && [[ -e ~/.guix-profile/bin/git ]]) || guix install python git")
         ("name" . "Bootstrap a host without python or git installed"))
        (("name" . "Clone dotfiles")
         ("git"
          ("repo" . "https://github.com/kitnil/dotfiles")
          ("dest" . "/home/oleg/.local/share/chezmoi")))
        (("uri"
          ("url" . "http://guix.wugi.info/guix/describe?channel=guix")
          ("return_content" . "yes"))
         ("register" . "channel")
         ("name" . "Describe current Guix"))
        (("name" . "Pull Guix")
         ("guix_pull"
          ("commit" . "{{channel.json.commit}}")
          ("channels" . "/home/oleg/.local/share/chezmoi/dotfiles/channels.scm")))
        (("name" . "Apply Guix manifest")
         ("guix_package"
          ("profile" . "/home/oleg/.guix-profile")
          ("manifest" . "/home/oleg/.local/share/chezmoi/dotfiles/manifests/{{ ansible_nodename }}.scm")
          ("load_path" . "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/modules")))))
     ("hosts" . #("guix_vm" "guix_work")))
    (("tasks"
      .
      #((("uri"
          ("url" . "http://guix.wugi.info/guix/describe?channel=guix")
          ("return_content" . "yes"))
         ("register" . "channel")
         ("name" . "Describe current Guix"))
        (("name" . "Pull Guix")
         ("guix_pull"
          ("commit" . "{{channel.json.commit}}")
          ("channels" . "/home/oleg/.local/share/chezmoi/dotfiles/channels.scm")))))
     ("hosts" . #("guix_vm" "guix_work"))
     ("become_flags" . "-i")
     ("become" . "yes"))))

(define (ansible-playbook playbook)
  (mlet* %store-monad ((main.json (text-file* "main.json"
                                              (with-output-to-string
                                                (lambda ()
                                                  (scm->json playbook #:pretty #t)))))
                       (program ->
                                (program-file
                                 "ansible-program"
                                 #~(invoke #$(file-append ansible "/bin/ansible-playbook")
                                           #$main.json))))
    (gexp->derivation "ansible-program" #~(symlink #$program #$output))))

(run-with-store (open-connection)
  (ansible-playbook %ansible-playbook))
