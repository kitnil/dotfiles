(use-modules (guile gitlab)
             (guix store))

(define %gitlab-projects
  (list (home-gitlab-project-configuration
         (name "notes")
         (description "My notes"))))

(define %terraform-state
  (string-append (getcwd) "/terraform.tfstate"))

(run-with-store (open-connection)
  (terraform-gitlab %gitlab-projects %terraform-state))
