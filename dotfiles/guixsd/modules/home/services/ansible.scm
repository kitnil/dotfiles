(define-module (home services ansible)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages mail)
  #:use-module (gnu services configuration)
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
  #:use-module (json builder)
  #:use-module (srfi srfi-1)
  #:export (ansible-playbook-service-type))

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
          ("dest" . "/home/oleg/ansible-out/files")))))
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

(define %ansible-playbook-mjru-router4.intr
  #((("tasks"
      .
      #((("include_vars" ("file" . "passwords.yml")))
        (("vars"
          ("ansible_ssh_pass"
           .
           "{{ router4_ansible_become_pass }}"))
         ("loop" . #("/root/.bash_history"))
         ("ignore_errors" . "yes")
         ("fetch"
          ("src" . "{{ item }}")
          ("dest" . "/home/oleg/ansible-out/files")))))
     ("hosts" . "router4.intr")
     ("gather_facts" . "no")
     ("become" . "yes"))))

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

(define %ansible-playbook-bq-main
  #((("tasks"
      .
      #((("loop" . #("/root/.bash_history"))
         ("ignore_errors" . "yes")
         ("fetch"
          ("src" . "{{ item }}")
          ("dest" . "/home/oleg/src/gitlab01.bqtstuff.com/devops/state")))
        (("shell" . "ip -json address")
         ("register" . "shell_result")
         ("ignore_errors" . "yes"))
        (("local_action"
          .
          "copy content='{{ shell_result.stdout | from_json | to_nice_json(indent=2) }}' dest='/home/oleg/src/gitlab01.bqtstuff.com/devops/state/{{ inventory_hostname }}/ip-address.json'"))))
     ("hosts" . "bq")
     ("gather_facts" . "yes"))))

(define-record-type* <ansible-playbook-configuration>
  ansible-playbook-configuration make-ansible-playbook-configuration
  ansible-playbook-configuration?
  (playbook            ansible-playbook-configuration-playbook)        ;scm
  (state-directory     ansible-playbook-configuration-state-directory) ;string
  (pre-hook            ansible-playbook-configuration-pre-hook         ;gexp
                       (default #~(begin #t)))
  (command             ansible-playbook-configuration-command          ;gexp
                       (default #~(invoke "ansible-playbook" "main.json"))))

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
                                       (define instance-dir
                                         (mkdtemp! "/tmp/ansible.XXXXXX"))
                                       (with-directory-excursion instance-dir
                                         (copy-file #$main.json "main.json")
                                         #$(ansible-playbook-configuration-pre-hook config)
                                         (guard (c ((invoke-error? c)
                                                    (report-invoke-error c)))
                                           #$(ansible-playbook-configuration-command config)))
                                       (delete-file-recursively instance-dir)
                                       (with-directory-excursion #$(ansible-playbook-configuration-state-directory config)
                                         (invoke "git" "add" "--all")
                                         (invoke "git" "commit" "--message=Update.")
                                         (invoke "git" "push")))))))
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
                 (with-output-to-file ".pass"
                   (lambda ()
                     (display #$(pass "show" "majordomo/public/majordomo/ansible-majordomo-history/passwords"))))))
            (command
             #~(invoke "ansible-playbook" "--vault-password-file=.pass" "-e@passwords.yml" "main.json"))))))
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
                 (with-output-to-file ".pass"
                   (lambda ()
                     (display #$(pass "show" "majordomo/public/majordomo/ansible-majordomo-history/passwords"))))))
            (command
             #~(invoke "ansible-playbook" "--vault-password-file=.pass" "-e@passwords.yml" "main.json"))))))
   #~(job
      '(next-hour '(18))
      #$(run-with-store (open-connection)
          (ansible-playbook
           (ansible-playbook-configuration
            (playbook %ansible-playbook-mjru-router4.intr)
            (state-directory
             (and=> (getenv "HOME")
                    (lambda (home)
                      (string-append home "/ansible-out/files"))))
            (pre-hook
             #~(begin
                 (copy-file #$(local-file "passwords.yml") "passwords.yml")
                 (with-output-to-file ".pass"
                   (lambda ()
                     (display #$(pass "show" "majordomo/public/majordomo/ansible-majordomo-history/passwords"))))))
            (command
             #~(invoke "ansible-playbook" "--vault-password-file=.pass" "-e@passwords.yml" "main.json"))))))
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
                 (with-output-to-file ".pass"
                   (lambda ()
                     (display #$(pass "show" "majordomo/public/majordomo/ansible-majordomo-history/passwords"))))))
            (command
             #~(invoke "ansible-playbook" "--vault-password-file=.pass" "-e@passwords.yml" "main.json"))))))
   #~(job
      '(next-hour '(20))
      #$(run-with-store (open-connection)
          (ansible-playbook
           (ansible-playbook-configuration
            (playbook %ansible-playbook-bq-main)
            (state-directory
             (and=> (getenv "HOME")
                    (lambda (home)
                      (string-append home "/src/gitlab01.bqtstuff.com/devops/state"))))
            (command #~(invoke "ansible-playbook" "--timeout=5" "main.json"))))))))

(define ansible-playbook-service-type
  (service-type
   (name 'ansible-playbook)
   (extensions
    (list (service-extension home-mcron-service-type
                             ansible-playbook-mcron-jobs)))
   (description
    "Periodically run ansible-playbook.")
   (default-value '())))
