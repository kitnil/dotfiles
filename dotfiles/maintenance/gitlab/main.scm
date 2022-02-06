(use-modules (guile gitlab)
             (guix store))

(define %gitlab-projects
  (list (home-gitlab-project-configuration
         (name "notes")
         (description "My notes"))
        (home-gitlab-project-configuration
         (name "prometheus-tp-link-exporter")
         (description "Prometheus TP Link Exporter"))))

(define %terraform-state
  (string-append (getcwd) "/terraform.tfstate"))

(run-with-store (open-connection)
  (terraform-gitlab %gitlab-projects %terraform-state))
