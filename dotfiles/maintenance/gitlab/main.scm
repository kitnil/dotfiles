(use-modules (guix build utils)
             (guix gexp)
             (guix store)
             (guix monads)
             (guix packages)
             (gnu packages bash)
             (guix modules)
             (gnu packages guile)
             (json builder))

(define %terraform-state
  (string-append (dirname (current-filename)) "/terraform.tfstate"))

(define %terraform-configuration
  `((variable
     (GITLAB_TOKEN
      (type . "string")))
    (provider
     (gitlab
      (token . "${var.GITLAB_TOKEN}")
      (base_url . "https://gitlab.com/api/v4/")))
    (resource
     (gitlab_project
      (notes
       (name . "notes")
       (default_branch . "master")
       (description . "My notes")
       (visibility_level . "public")
       (wiki_enabled . #false)
       (snippets_enabled . #false)
       (issues_enabled . #true)
       (container_registry_enabled . #false)
       (shared_runners_enabled . #false)
       (request_access_enabled . #false)
       (pipelines_enabled . #true))))))

(define (terraform-gitlab)
  (mlet* %store-monad ((main.tf.json (text-file* "main.tf.json"
                                                 (with-output-to-string
                                                   (lambda ()
                                                     (scm->json
                                                      %terraform-configuration
                                                      #:pretty #t)))))
                       (program ->
                                (program-file
                                 "terraform-gitlab-program"
                                 (with-imported-modules '((guix build syscalls)
                                                          (guix build utils))
                                   #~(begin
                                       (use-modules (guix build syscalls)
                                                    (guix build utils))
                                       (define instance-dir
                                         (mkdtemp! "/tmp/terraform.XXXXXX"))
                                       (chdir instance-dir)
                                       (copy-file #$main.tf.json "main.tf.json")
                                       (invoke "terraform" "init")
                                       (invoke "terraform" "apply"
                                               #$(string-append
                                                  "-state=" %terraform-state))
                                       (delete-file-recursively instance-dir))))))
    (gexp->derivation "terraform-gitlab" #~(symlink #$program #$output))))

(run-with-store (open-connection) (terraform-gitlab))
