
;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (wigust packages documentation)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system copy))

(define-public slides-linux-perf-tools
  (package
    (name "slides-linux-perf-tools")
    (version "2015")
    (source
     (origin
       (method url-fetch)
       (uri "https://cdn.oreillystatic.com/en/assets/1/event/122/Linux%20perf%20tools%20Presentation.pdf")
       (file-name (string-append name "-" version ".pdf"))
       (sha256
        (base32
         "03xjl7bdhjw8v3ikajyyxnqyph35d8ca83yb6skxs7xvlsw11hg5"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `((,(assoc-ref %build-inputs "source")
          ,(let ((title (string-drop ,name (string-length "slides-"))))
             (string-append "/share/doc/" title "/" title ".pdf"))))
       #:phases (modify-phases %standard-phases (delete 'unpack))))
    (home-page "https://github.com/jenkinsci/configuration-as-code-plugin")
    (synopsis "Tutorial by Brendan Gregg Velocity conference 2015 Santa Clara")
    (description "This tutorial explains methodologies for using these tools,
and provides a tour of four tool types: observability, benchmarking, tuning,
and static tuning. Many tools will be discussed, including top, iostat,
tcpdump, sar, perf_events, ftrace, SystemTap, sysdig, and others, as well
observability frameworks in the Linux kernel: PMCs, tracepoints, kprobes, and
uprobes.")
    (license #f)))

(define-public slides-devops-world-jenkins-casc
  (package
    (name "slides-devops-world-jenkins-casc")
    (version "2018")
    (source
     (origin
       (method url-fetch)
       (uri "https://static.sched.com/hosted_files/devopsworldjenkinsworld2018/1b/Jenkins CasC JW18.pdf")
       (file-name (string-append name "-" version ".pdf"))
       (sha256
        (base32
         "0qj3n7z47cbl20xxa7jivwmp272qhvcmvw9cdxzp577yd6ma9b4a"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `((,(assoc-ref %build-inputs "source")
          ,(let ((title (string-drop ,name (string-length "slides-"))))
             (string-append "/share/doc/" title "/" title ".pdf"))))
       #:phases (modify-phases %standard-phases (delete 'unpack))))
    (home-page "https://github.com/jenkinsci/configuration-as-code-plugin")
    (synopsis "Introduction Jenkins Configuration as Code Plugin")
    (description "This package provides slides for a presention DevOPS World
2018 Jenkins CasC.")
    (license #f)))

(define-public slides-concise-gnu-bash
  (package
    (name "slides-concise-gnu-bash")
    (version "2017")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://talk.jpnc.info/bash_lnfw_"
                                  version ".pdf"))
              (file-name (string-append name "-" version ".pdf"))
              (sha256
               (base32
                "1v8nn3p7qiibsmbigdcv8q40pgsq6s8v63193f7qq5y2yhrqml7a"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `((,(assoc-ref %build-inputs "source")
          ,(let ((title (string-drop ,name (string-length "slides-"))))
             (string-append "/share/doc/" title "/" title ".pdf"))))
       #:phases (modify-phases %standard-phases (delete 'unpack))))
    (home-page "http://talk.jpnc.info/")
    (synopsis "Introduction to Bash advances usage")
    (description "This package provides slides for a presention Introduction
to Bash advances usage.")
    (license #f)))

(define-public documentation-arcconf
  (package
    (name "documentation-arcconf")
    (version "3_00_23484_ug")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.adaptec.com/pdfs/user_guides/microsemi_cli_smarthba_smartraid_v"
                                  version ".pdf"))
              (file-name (string-append name "-" version ".pdf"))
              (sha256
               (base32
                "0x2bzi1ywpin8504ra9zlzh5aij15gqgfmjj1b5kylaap6vb92xb"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `((,(assoc-ref %build-inputs "source")
          ,(let ((title (string-drop ,name (string-length "documentation-"))))
             (string-append "/share/doc/" title "/" title ".pdf"))))
       #:phases (modify-phases %standard-phases (delete 'unpack))))
    (home-page "http://download.adaptec.com/pdfs/user_guides/")
    (synopsis "ARCCONF Command Line Utility")
    (description "Microsemi Smart Storage Controllers User's Guide.")
    (license #f)))

(define-public documentation-supermicro-3u-mnl-2197
  (package
    (name "documentation-supermicro-superserver-3u-mnl-2197")
    (version "0.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.supermicro.com/manuals/superserver/3U/MNL-2197.pdf"))
              (file-name (string-append name "-" version ".pdf"))
              (sha256
               (base32
                "0i7bbhp1nqv31q4nhd47gcig5jbi0zdwaxvdkcf2rbv27409sb9f"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `((,(assoc-ref %build-inputs "source")
          ,(let ((title (string-drop ,name (string-length "documentation-"))))
             (string-append "/share/doc/" title "/" title ".pdf"))))
       #:phases (modify-phases %standard-phases (delete 'unpack))))
    (home-page "http://download.adaptec.com/pdfs/user_guides/")
    (synopsis "superserver-3u-mnl-2197")
    (description "superserver-3u-mnl-2197")
    (license #f)))

(define-public book-sicp
  (package
    (name "book-sicp")
    (version "1996")
    (source (origin
              (method url-fetch)
              (uri "https://web.mit.edu/alexmv/6.037/sicp.pdf")
              (file-name (string-append name "-" version ".pdf"))
              (sha256
               (base32
                "12yszs2pwpas9a6643j5f4z1a0b872jz9i19zpb130vxas3rlw08"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `((,(assoc-ref %build-inputs "source")
          ,(let ((title (string-drop ,name (string-length "book-"))))
             (string-append "/share/doc/" title "/" title ".pdf"))))
       #:phases (modify-phases %standard-phases (delete 'unpack))))
    (home-page "https://mit.edu/")
    (synopsis "Structure and Interpretation of Computer Programs, second edition")
    (description "This package provides a PDF version of book Structure and
Interpretation of Computer Programs, second edition.")
    (license #f)))

(define-public cheatsheet-gdb
  (let ((commit "4c0f68b82fc65e24d4c7e96c487a6fcd9510e9c4")
        (revision "1"))
    (package
      (name "cheatsheet-gdb")
      (version (git-version "0.0.1" revision commit)) ;no upstream release
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/hellogcc/100-gdb-tips")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "01cmn3d8dfq36jblc54lahw0vhl96kzj9kcyp4kbqbpq2gfivi96"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan
         `((,(string-append (assoc-ref %build-inputs "source") "/refcard.pdf")
            ,(let ((title (string-drop ,name (string-length "cheatsheet-"))))
               (string-append "/share/doc/" title "/" title ".pdf"))))
         #:phases (modify-phases %standard-phases (delete 'unpack))))
      (home-page "https://github.com/hellogcc/100-gdb-tips/")
      (synopsis "Collection of GDB tips.  100 maybe just mean many here.")
      (description "This package provides a collection of GDB tips.  100 maybe
just mean many here.")
      (license #f))))
