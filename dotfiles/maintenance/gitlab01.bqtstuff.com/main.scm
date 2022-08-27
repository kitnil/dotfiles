(use-modules (guile gitlab)
             (guix store))

(define %gitlab-projects
  (list (home-gitlab-project-configuration
         (name "password-store")
         (description "https://www.passwordstore.org/"))))

(define %terraform-state
  (string-append (getcwd) "/terraform.tfstate"))

(define %terraform-configuration
  `((variable
     (GITLAB_TOKEN
      (type . "string")))
    (provider
     (gitlab
      (token . "${var.GITLAB_TOKEN}")
      (base_url . "https://gitlab01.bqtstuff.com/api/v4/")))))

(run-with-store (open-connection)
  (terraform-gitlab (terraform-gitlab-configuration
                     (projects %gitlab-projects)
                     (state %terraform-state)
                     (terraform-configuration %terraform-configuration))))
