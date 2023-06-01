(define-module (home services ansible)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services configuration)
  #:use-module (gnu services mcron)
  #:use-module (gnu home services mcron)
  #:use-module (guile pass)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (json builder)
  #:use-module (srfi srfi-1)
  #:export (ansible-playbook-service-type))

(define git-binary
  (file-append git "/bin/git"))

(define ansible-playbook-binary
  (file-append ansible-core "/bin/ansible-playbook"))

(define %ansible-playbook-mjru-main
  #((("tasks"
      .
      #((("include_vars" ("file" . "passwords.yml")))
        (("when"
          .
          "'router' not in group_names and 'deprecated' not in group_names")
         ("vars"
          ("ansible_become_pass"
           .
           "{{ eng_ansible_become_pass }}"))
         ("loop"
          .
          #("/root/.bash_history" "/root/.mysql_history"))
         ("ignore_errors" . "yes")
         ("fetch"
          ("src" . "{{ item }}")
          ("dest" . "/home/oleg/ansible-out/files")))
        (("shell" . "dmidecode")
         ("register" . "shell_result")
         ("ignore_errors" . "yes")
         ("become" . "yes"))
        (("local_action"
          .
          "copy content='{{ shell_result.stdout }}' dest='/home/oleg/ansible-out/files/{{ ansible_hostname }}.intr/dmidecode.txt'"))))
     ("hosts" . "majordomo")
     ("gather_facts" . "no")
     ("become" . "yes"))))

(define %ansible-playbook-mjru-raid
  #((("vars"
      ("ansible_become_pass"
       .
       "{{ eng_ansible_become_pass }}"))
     ("tasks"
      .
      #((("when"
          .
          "'router' not in group_names and 'deprecated' not in group_names")
         ("shell"
          .
          "arcconf getconfig 1 && arcconf getstatus 1 || MegaCli64 -AdpAllinfo -aALL")
         ("register" . "shell_result")
         ("ignore_errors" . "yes")
         ("become" . "yes"))
        (("local_action"
          .
          "copy content='{{ shell_result.stdout }}' dest='/home/oleg/ansible-out/files/{{ ansible_hostname }}.intr/raid.log'"))
        (("when"
          .
          "'router' not in group_names and 'deprecated' not in group_names")
         ("shell"
          .
          "for dev in /dev/sg*; do echo -e \"\\n\\n@ $dev\"; smartctl -a $dev -d sat; done")
         ("register" . "smartctl_result")
         ("ignore_errors" . "yes")
         ("become" . "yes"))
        (("local_action"
          .
          "copy content='{{ smartctl_result.stdout }}' dest='/home/oleg/ansible-out/files/{{ ansible_hostname }}.intr/smartctl.log'"))))
     ("hosts" . #("kvm" "web")))))

(define %ansible-playbook-mjru-deprecated
  #((("tasks"
      .
      #((("include_vars" ("file" . "passwords.yml")))
        (("vars"
          ("ansible_become_pass"
           .
           "{{ eng_ansible_become_pass }}"))
         ("register" . "output")
         ("raw" . "cat /root/.bash_history")
         ("become" . "yes"))
        (("set_fact"
          .
          "remote_host=\"{{ ansible_host }}\""))
        (("local_action"
          .
          "file path=\"/home/oleg/ansible-out/files/{{ remote_host }}/root\" state=directory"))
        (("local_action"
          .
          "copy content=\"{{ output.stdout }}\" dest=\"/home/oleg/ansible-out/files/{{ remote_host }}/root/.bash_history\""))))
     ("hosts" . "deprecated")
     ("gather_facts" . "no"))))

(define-record-type* <ansible-playbook-configuration>
  ansible-playbook-configuration make-ansible-playbook-configuration
  ansible-playbook-configuration?
  (playbook            ansible-playbook-configuration-playbook)        ;scm
  (state-directory     ansible-playbook-configuration-state-directory) ;string
  (pre-hook            ansible-playbook-configuration-pre-hook         ;gexp
                       (default #~(begin #t)))
  (post-hook           ansible-playbook-configuration-post-hook        ;gexp
                       (default #~(begin #t)))
  (command             ansible-playbook-configuration-command          ;gexp
                       (default #~(invoke #$ansible-playbook-binary "main.json"))))

(define (ansible-playbook config)
  (mlet* %store-monad ((main.json (text-file* "main.json"
                                              (with-output-to-string
                                                (lambda ()
                                                  (scm->json (ansible-playbook-configuration-playbook config))))))
                       (program ->
                                (program-file
                                 "ansible-program"
                                 (with-imported-modules '((guix build syscalls)
                                                          (guix build utils))
                                   #~(begin
                                       (use-modules (guix build syscalls)
                                                    (guix build utils)
                                                    (srfi srfi-34))
                                       (setgroups '#())
                                       (let ((pw (getpwnam "oleg")))
                                         (setgid (passwd:gid pw))
                                         (setuid (passwd:uid pw)))
                                       (setenv "HOME" "/home/oleg") ;do not hardcode

                                       ;; XXX: LANG environment fixes ansible-playbook.
                                       ;; ERROR: Ansible requires the locale encoding to be UTF-8; Detected None.
                                       (setenv "LANG" "en_US.UTF-8")

                                       (define instance-dir
                                         (mkdtemp! "/tmp/ansible.XXXXXX"))
                                       (with-directory-excursion instance-dir
                                         (copy-file #$main.json "main.json")
                                         #$(ansible-playbook-configuration-pre-hook config)
                                         (guard (c ((invoke-error? c)
                                                    (report-invoke-error c)))
                                           #$(ansible-playbook-configuration-command config)))
                                       (delete-file-recursively instance-dir)
                                       #$(ansible-playbook-configuration-post-hook config))))))
    (gexp->derivation "ansible-program" #~(symlink #$program #$output))))

(define (ansible-playbook-mcron-jobs config)
  (list
   #~(job
      '(next-hour '(16))
      #$(run-with-store (open-connection)
          (ansible-playbook
           (ansible-playbook-configuration
            (playbook %ansible-playbook-mjru-main)
            (state-directory
             (and=> (getenv "HOME")
                    (lambda (home)
                      (string-append home "/ansible-out/files"))))
            (pre-hook
             #~(begin
                 (copy-file #$(local-file "passwords.yml") "passwords.yml")
                 (copy-file "/etc/guix/secrets/ansible" ".pass")))
            (command
             #~(invoke #$ansible-playbook-binary "--vault-password-file=.pass" "-e@passwords.yml" "main.json"))
            (post-hook
             #~(with-directory-excursion #$state-directory
                 (invoke #$git-binary "add" "--all")
                 (invoke #$git-binary "commit" "--message=Update.")))))))
   #~(job
      '(next-hour '(17))
      #$(run-with-store (open-connection)
          (ansible-playbook
           (ansible-playbook-configuration
            (playbook %ansible-playbook-mjru-raid)
            (state-directory
             (and=> (getenv "HOME")
                    (lambda (home)
                      (string-append home "/ansible-out/files"))))
            (pre-hook
             #~(begin
                 (copy-file #$(local-file "passwords.yml") "passwords.yml")
                 (copy-file "/etc/guix/secrets/ansible" ".pass")))
            (command
             #~(invoke #$ansible-playbook-binary "--vault-password-file=.pass" "-e@passwords.yml" "main.json"))
            (post-hook
             #~(with-directory-excursion #$state-directory
                 (invoke #$git-binary "add" "--all")
                 (invoke #$git-binary "commit" "--message=Update.")))))))
   #~(job
      '(next-hour '(19))
      #$(run-with-store (open-connection)
          (ansible-playbook
           (ansible-playbook-configuration
            (playbook %ansible-playbook-mjru-deprecated)
            (state-directory
             (and=> (getenv "HOME")
                    (lambda (home)
                      (string-append home "/ansible-out/files"))))
            (pre-hook
             #~(begin
                 (copy-file #$(local-file "passwords.yml") "passwords.yml")
                 (copy-file "/etc/guix/secrets/ansible" ".pass")))
            (command
             #~(invoke #$ansible-playbook-binary "--vault-password-file=.pass" "-e@passwords.yml" "main.json"))
            (post-hook
             #~(with-directory-excursion #$state-directory
                 (invoke #$git-binary "add" "--all")
                 (invoke #$git-binary "commit" "--message=Update.")))))))))

(define ansible-playbook-service-type
  (service-type
   (name 'ansible-playbook)
   (extensions
    (list (service-extension mcron-service-type
                             ansible-playbook-mcron-jobs)))
   (description
    "Periodically run ansible-playbook.")
   (default-value '())))
