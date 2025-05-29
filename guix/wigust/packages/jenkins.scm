(define-module (wigust packages jenkins)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:))

(define-public jenkins
  (package
    (name "jenkins")
    (version "2.277.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://get.jenkins.io/war-stable/"
                                  version "/jenkins.war"))
              (sha256
               (base32
                "08lv5v5kxp9ln798gjmh8j9a8r8xc471fbhiz2l7gxncpxn50ga2"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `((,(assoc-ref %build-inputs "source")
          ,(string-append "/webapps/jenkins.war")))
       #:phases (modify-phases %standard-phases (delete 'unpack))))
    (home-page "https://www.jenkins.io/")
    (synopsis "Continuous integration tool")
    (description
     "This package provides a Jenkins continuous integration tool.")
    (license license:expat)))
