(use-modules (guile gitlab)
             (guix store))

(define %gitlab-projects
  (list (home-gitlab-project-configuration
         (name "notes")
         (description "My notes"))
        (home-gitlab-project-configuration
         (name "prometheus-tp-link-exporter")
         (description "Prometheus TP Link Exporter"))
        (home-gitlab-project-configuration
         (name "python-prometheus-ssh-exporter")
         (description "Prometheus SSH Exporter written in Python"))
        (home-gitlab-project-configuration
         (name "githunt")
         (description "Hunt the most starred projects on any date on GitHub.  Fork of https://github.com/kamranahmedse/githunt"))
        (home-gitlab-project-configuration
         (name "kubernetes-helm")
         (description "Kubernetes Helm repository"))))

(define %terraform-state
  (string-append (getcwd) "/terraform.tfstate"))

(run-with-store (open-connection)
  (terraform-gitlab (terraform-gitlab-configuration
                     (projects %gitlab-projects)
                     (state %terraform-state))))
