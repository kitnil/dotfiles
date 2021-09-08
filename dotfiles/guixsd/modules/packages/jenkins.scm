(define-module (packages jenkins)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:))

(define-public jenkins
  (package
    (name "jenkins")
    (version "2.289.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://get.jenkins.io/war-stable/"
                                  version "/jenkins.war"))
              (sha256
               (base32
                "11wb4kqy1hja2fgnqsr6p0khdyvinclprxz9z5m58czrsllzsvcr"))))
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
