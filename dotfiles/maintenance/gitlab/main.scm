(use-modules (guix build utils)
             (guix gexp)
             (guix store)
             (guix monads)
             (guix packages)
             (gnu packages bash)
             (guix modules)
             (gnu packages guile)
             (json builder)
             (guix records)
             (gnu services configuration)
             (srfi srfi-1))

(define %terraform-state
  (string-append (dirname (current-filename)) "/terraform.tfstate"))

(define (uglify-field-name field-name)
  "Convert symbol FIELD-NAME to a camel case string.
@code{symbol-name} => \"@code{symbolName}\"."
  (let* ((str (string-delete #\? (symbol->string field-name)))
         (spl-str (string-split str #\-)))
    (string-join spl-str "_")))

(define (serialize-field field-name val)
  (cons (uglify-field-name field-name) val))

(define serialize-boolean serialize-field)

(define serialize-string serialize-field)

(define-configuration home-gitlab-project-configuration
  (name
   (string (configuration-missing-field 'home-gitlab-project-configuration 'name))
   "Name of the project.")
  (default-branch
   (string "master")
   "Default branch for the project.")
  (description
   (string (configuration-missing-field 'home-gitlab-project-configuration 'description))
   "Description of the project.")
  (visibility-level
   (string "public")
   "Project access level.")
  (wiki-enabled?
   (boolean #f)
   "Enable wiki for the project.")
  (snippets-enabled?
   (boolean #f)
   "Enable snippets for the project.")
  (issues-enabled?
   (boolean #t)
   "Enable issue tracking for the project.")
  (container-registry-enabled?
   (boolean #f)
   "Enable container registry for the project.")
  (shared-runners-enabled?
   (boolean #f)
   "Enable shared runners for this project.")
  (request-access-enabled?
   (boolean #f)
   "Allow users to request member access.")
  (pipelines-enabled?
   (boolean #f)
   "Enable pipelines for the project."))

(define (serialize-gitlab-project-configuration config fields)
  "Similar to serialize-configuration from gnu/services/configuration.scm."
  (plain-file
   (string-append "gitlab-project-"
                  (home-gitlab-project-configuration-name config) ".tf.json")
   (with-output-to-string
     (lambda ()
       (scm->json
        `((resource
           (gitlab_project
            (,(home-gitlab-project-configuration-name config)
             ,@(map (lambda (field)
                      ((configuration-field-serializer field)
                       (configuration-field-name field)
                       ((configuration-field-getter field) config)))
                    fields)))))
        #:pretty #t)))))

(define %terraform-configuration
  `((variable
     (GITLAB_TOKEN
      (type . "string")))
    (provider
     (gitlab
      (token . "${var.GITLAB_TOKEN}")
      (base_url . "https://gitlab.com/api/v4/")))))

(define %gitlab-projects
  (list (home-gitlab-project-configuration
         (name "notes")
         (description "My notes"))
        (home-gitlab-project-configuration
         (name "ansible-majordomo-history")
         (description "Jenkins pipeline which runs Ansible to fetch various information from servers"))))

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
                                       (for-each
                                        (lambda (file)
                                          (install-file file "."))
                                        '#$(map (lambda (project)
                                                  (serialize-gitlab-project-configuration project home-gitlab-project-configuration-fields))
                                                %gitlab-projects))
                                       (invoke "terraform" "init")
                                       (invoke "terraform" "apply"
                                               #$(string-append
                                                  "-state=" %terraform-state))
                                       (delete-file-recursively instance-dir))))))
    (gexp->derivation "terraform-gitlab" #~(symlink #$program #$output))))

(run-with-store (open-connection) (terraform-gitlab))
