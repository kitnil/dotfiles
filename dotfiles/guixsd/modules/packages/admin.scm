(define-module (packages admin)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (guix build utils)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages python-xyz))

(define-public crowdsec
  (package
    (name "crowdsec")
    (version "1.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/crowdsecurity/crowdsec/releases/download/v"
                                  version "/crowdsec-release.tgz"))
              (sha256
               (base32
                "02lapak173y9c96jwghwq9fm6yzjmzcqzil4b5hrr8v46044rl2l"))))
    (build-system trivial-build-system)
    (inputs
     (list bash-minimal gzip tar glibc patchelf))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (setenv "PATH"
                 (string-append
                  (assoc-ref %build-inputs "bash-minimal") "/bin"
                  ":" (assoc-ref %build-inputs "gzip") "/bin"
                  ":" (assoc-ref %build-inputs "tar") "/bin"))
         (invoke "tar" "--strip-components=1" "-xf"
                 (assoc-ref %build-inputs "source"))
         (mkdir-p (string-append %output "/bin"))
         (copy-file "cmd/crowdsec/crowdsec"
                    (string-append %output "/bin/crowdsec"))
         (wrap-program (string-append %output "/bin/crowdsec")
           `("LD_LIBRARY_PATH" ":" prefix (,(string-append (assoc-ref %build-inputs "glibc")
                                                           "/lib"))))
         (copy-file "cmd/crowdsec-cli/cscli"
                    (string-append %output "/bin/cscli"))
         (mkdir-p (string-append %output "/share/crowdsec/plugins"))
         (copy-file "cmd/notification-email/notification-email"
                    (string-append %output "/share/crowdsec/plugins/notification-email"))
         (copy-file "cmd/notification-http/notification-http"
                    (string-append %output "/share/crowdsec/plugins/notification-http"))
         (copy-file "cmd/notification-slack/notification-slack"
                    (string-append %output "/share/crowdsec/plugins/notification-slack")))))
    (home-page "https://crowdsec.net/")
    (synopsis "Collaborative behavior detection engine")
    (description "CrowdSec is a free, modern & collaborative behavior
detection engine, coupled with a global IP reputation network. It stacks on
fail2ban's philosophy but is IPV6 compatible and 60x faster (Go vs Python),
uses Grok patterns to parse logs and YAML scenario to identify
behaviors. CrowdSec is engineered for modern Cloud / Containers / VM based
infrastructures (by decoupling detection and remediation). Once detected you
can remedy threats with various bouncers (firewall block, nginx http 403,
Captchas, etc.) while the aggressive IP can be sent to CrowdSec for curation
before being shared among all users to further improve everyone's
security. See FAQ or read below for more.")
    (license license:expat)))

(define-public crowdsec-firewall-bouncer
  (package
    (name "crowdsec-firewall-bouncer")
    (version "0.0.22")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/crowdsecurity/cs-firewall-bouncer/releases/download/v"
         version "/crowdsec-firewall-bouncer.tgz"))
       (sha256
        (base32
         "19vk6l8n6d1x4gd2pkf9617znnq0s2y4x9iq7m07lhabxgknnd70"))))
    (build-system trivial-build-system)
    (inputs
     (list gzip tar glibc patchelf))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (setenv "PATH"
                 (string-append
                  (assoc-ref %build-inputs "gzip") "/bin"
                  ":" (assoc-ref %build-inputs "tar") "/bin"
                  ":" (assoc-ref %build-inputs "patchelf") "/bin"))
         (invoke "tar" "--strip-components=1" "-xf"
                 (assoc-ref %build-inputs "source"))
         (mkdir-p (string-append %output "/bin"))
         (copy-file "crowdsec-firewall-bouncer"
                    (string-append %output "/bin/crowdsec-firewall-bouncer"))
         (invoke "patchelf" "--set-interpreter"
                 (string-append (assoc-ref %build-inputs "glibc")
                                "/lib/ld-linux-x86-64.so.2")
                 (string-append %output "/bin/crowdsec-firewall-bouncer")))))
    (home-page "https://doc.crowdsec.net/docs/bouncers/firewall/")
    (synopsis "Crowdsec bouncer written in golang for firewalls")
    (description "crowdsec-firewall-bouncer will fetch new and old decisions
from a CrowdSec API to add them in a blocklist used by supported firewalls.")
    (license license:expat)))

(define-public osquery
  (package
    (name "osquery")
    (version "5.2.3")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "https://github.com/osquery/osquery/releases/download/"
                version "/osquery-" version "_1.linux_x86_64.tar.gz"))
              (sha256
               (base32
                "1x2k93zh04ykhvh3r4vzz90cjxv7p6paw8bpqd63mfdg9gr6axb7"))))
    (build-system trivial-build-system)
    (inputs (list gzip tar glibc patchelf zlib))
    (native-inputs `(("source" ,source)))
    (outputs '("out" "opt"))
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (setenv "PATH"
                  (string-append
                   #$(this-package-input "gzip") "/bin"
                   ":" #$(this-package-input "tar") "/bin"
                   ":" #$(this-package-input "patchelf") "/bin"))
          (invoke "tar" "-xf" #$(this-package-native-input "source"))
          (mkdir-p (string-append #$output "/bin"))
          (copy-file "opt/osquery/bin/osqueryd"
                     (string-append #$output "/bin/osqueryd"))
          (invoke "patchelf"
                  "--set-interpreter"
                  (string-append #$(this-package-input "glibc")
                                 "/lib/ld-linux-x86-64.so.2")
                  "--set-rpath"
                  (string-append #$(this-package-input "zlib")
                                 "/lib")
                  (string-append #$output "/bin/osqueryd"))
          (delete-file-recursively "opt/osquery/bin")
          (symlink (string-append #$output "/bin/osqueryd")
                   (string-append #$output "/bin/osqueryi"))
          (copy-recursively "opt/osquery" #$output:opt))))
    (home-page "https://osquery.io/")
    (synopsis "Expose an operating system as a relational database")
    (description "osquery is a SQL powered operating system instrumentation,
monitoring, and analytics.")
    (license license:expat)))

(define-public spacer
  (package
    (name "spacer")
    (version "0.1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/samwho/spacer/releases/download/v"
                                  version "/spacer-x86_64-unknown-linux-gnu.tar.gz"))
              (sha256
               (base32
                "064m1v1lm4iknyfbnqs6g3n7y4fd7iadx8fzdf16r69016ah8b0z"))))
    (build-system trivial-build-system)
    (inputs (list gzip tar glibc patchelf `(,gcc "lib")))
    (native-inputs `(("source" ,source)))
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (setenv "PATH"
                  (string-append
                   #$(this-package-input "gzip") "/bin"
                   ":" #$(this-package-input "tar") "/bin"
                   ":" #$(this-package-input "patchelf") "/bin"))
          (invoke "tar" "-xf" #$(this-package-native-input "source"))
          (mkdir-p (string-append #$output "/bin"))
          (copy-file "spacer"
                     (string-append #$output "/bin/spacer"))
          (invoke "patchelf"
                  "--set-interpreter"
                  (string-append #$(this-package-input "glibc")
                                 "/lib/ld-linux-x86-64.so.2")
                  "--set-rpath"
                  (dirname (search-input-file %build-inputs "/lib/libgcc_s.so.1"))
                  (string-append #$output "/bin/spacer")))))
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))
