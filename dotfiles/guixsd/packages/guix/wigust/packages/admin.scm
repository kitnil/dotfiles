;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2019, 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (wigust packages admin)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg))

(define-public pscircle
  (package
    (name "pscircle")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://gitlab.com/mildlyparallel/pscircle/-/archive/v"
             version "/pscircle-v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1k757yf2bmgfrjd417l6kpcf83hlvi0z1791vz967mwcklrsb3fj"))))
    (build-system meson-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("cairo" ,cairo)
       ("libpng" ,libpng)
       ("libx11" ,libx11)))
    (home-page "https://gitlab.com/mildlyparallel/pscircle")
    (synopsis "Visualize Linux processes in a form of radial tree")
    (description
     "@code{pscircle} visualizes Linux processes in a form of radial tree")
    (license license:gpl2+)))

(define-public with
  (let ((commit "28eb40bbc08d171daabf0210f420477ad75e16d6"))
    (package
      (name "with")
      (version "0.0.1")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/mchav/with.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0y8m2z4nbq67iy95b581rdfz2hss37fbgxfna66qzz1xgcnfr8cq"))))
      (build-system trivial-build-system)
      (inputs
       `(("bash" ,bash)))
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (copy-recursively (assoc-ref %build-inputs "source") ".")
           (substitute* "with"
             (("/usr/bin/env bash") (string-append (assoc-ref %build-inputs "bash")
                                                   "/bin/bash")))
           (install-file "with" (string-append %output "/bin"))
           (mkdir-p (string-append %output "/etc/bash_completion.d"))
           (copy-file "with.bash-completion"
                      (string-append %output "/etc/bash_completion.d/with"))
           #t)))
      (home-page "https://github.com/mchav/with/")
      (synopsis "Command prefixing for continuous workflow using a single tool")
      (description "This package provides command prefixing for continuous workflow using a single tool.")
      (license license:asl2.0))))

(define-public git-quick-stats
  (package
    (name "git-quick-stats")
    (version "2.0.11")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/arzzen/git-quick-stats.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "19chwnc936bxf0bnxsvw6nhfxnj0216jx9ajjckw3q440l932799"))))
    (build-system trivial-build-system)
    (inputs
       `(("bash" ,bash)))
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (copy-recursively (assoc-ref %build-inputs "source") ".")
           (substitute* "git-quick-stats"
             (("/usr/bin/env bash") (string-append (assoc-ref %build-inputs "bash")
                                                   "/bin/bash")))
           (install-file "git-quick-stats" (string-append %output "/bin"))
           #t)))
    (home-page "https://github.com/arzzen/git-quick-stats/")
    (synopsis "Git quick statistics")
    (description "This package provides a Git quick statistics is a simple and
efficient way to access various statistics in git repository.")
    (license license:expat)))

(define-public ok-sh
  (package
    (name "ok-sh")
    (version "0.5.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/whiteinge/ok.sh.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0zg9cmqblf1m73vrikksrc0cwjqspa351b9l3jgc6dxrrd6f1x81"))))
    (build-system trivial-build-system)
    (inputs
     `(("bash" ,bash)
       ("gawk" ,gawk)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (copy-recursively (assoc-ref %build-inputs "source") ".")
         (substitute* "ok.sh"
           (("/usr/bin/env sh") (string-append (assoc-ref %build-inputs "bash")
                                               "/bin/bash"))
           (("awk ") (string-append (assoc-ref %build-inputs "gawk")
                                    "/bin/gawk ")))
         (install-file "ok.sh" (string-append %output "/bin"))
         #t)))
    (home-page "https://github.com/whiteinge/ok.sh/")
    (synopsis "Bourne shell GitHub API client library focused on interfacing with shell scripts")
    (description "This package provides a Bourne shell GitHub API client
library focused on interfacing with shell scripts.")
    (license license:bsd-3)))

(define-public git-splits
  (package
    (name "git-splits")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ajdruff/git-splits.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "14cxcivr10aq5399hf102sbp4gimww7jqr669mbxfly1w7y4yv6y"))))
    (build-system trivial-build-system)
    (inputs
     `(("bash" ,bash)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (copy-file (string-append (assoc-ref %build-inputs "source") "/git-splits")
                    "git-splits")
         (substitute* "git-splits"
           (("/usr/bin/env bash")
            (string-append (assoc-ref %build-inputs "bash") "/bin/bash")))
         (install-file "git-splits" (string-append %output "/bin"))
         #t)))
    (home-page "https://github.com/ajdruff/git-splits/")
    (synopsis "Extracts multiple directories of a git repo into a new branch")
    (description "git-splits - Extracts directories into a new branch
with re-written history containing only those directories.")
    (license license:expat)))

(define-public slacktee
  (package
    (name "slacktee")
    (version "1.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/coursehero/slacktee.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "02dxp2gsgc781adlzbx7mhkw8ra22vkd76z53qcd0cdynygw8546"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `((,(string-append (assoc-ref %build-inputs "source") "/slacktee.sh")
          ,(string-append "/bin/slacktee")))
       #:phases (modify-phases %standard-phases (delete 'unpack))))
    (home-page "https://github.com/coursehero/slacktee/")
    (synopsis "Bash script that works like tee command with Slack")
    (description "This package provides a ")
    (license license:asl2.0)))
