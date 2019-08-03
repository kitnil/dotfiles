;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2019 Oleg Pykhalov <go.wigust@gmail.com>

(define-module (wigust packages zabbix)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages package-management)
  #:use-module (guix build utils)
  #:use-module (guix build-system guile)
  #:use-module (guix build-system trivial)
  #:use-module (guix git-download)
  #:use-module (guix))

(define-public zabbix-tcp-service-discovery
  (package
    (name "zabbix-tcp-service-discovery")
    ;; No upstream version.
    (version "0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.zabbix.org/mw/images/c/c3/Tcp-service-discovery.sh"))
              (file-name (string-append name "-" version ".sh"))
              (sha256
               (base32
                "1v3ywyyb04fhbb9263s7l2x73i3xllnmnb32d1ljkaifvq588hca"))))
    (build-system trivial-build-system)
    (inputs
     `(("bash" ,bash)
       ("nmap" ,nmap)))
    (arguments
     `(#:modules
       ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((script (string-append ,name)))
           (copy-file (assoc-ref %build-inputs "source") script)
           (substitute* script
             (("/bin/sh") (string-append (assoc-ref %build-inputs "bash")
                                         "/bin/bash"))
             (("nmap")
              (string-append (assoc-ref %build-inputs "nmap") "/bin/nmap")))
           (chmod script #o555)
           (install-file script (string-append %output "/bin")))
         #t)))
    (home-page "https://www.zabbix.org/wiki/Discovery_of_TCP_services")
    (synopsis "The port scanning script uses nmap, install and verify that it works. ")
    (description "Shell script that will return all the TCP ports that appear open the monitored host.")
    (license #f)))

(define-public zabbix-guix
  (let ((commit "83b4647ef84781cd6630e053589ddc101f840289"))
    (package
      (name "zabbix-guix")
      (version (git-version "0.0.1" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://anongit.duckdns.org/guix/zabbix-guix.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0k2559r7x99xv3jv47qlkphfnmz54z7wvrvg4xdjw24p41zjjg8m"))))
      (build-system guile-build-system)
      (inputs
       `(("bash" ,bash)))
      (native-inputs
       `(("guile" ,guile-2.2)
         ("guile-json" ,guile-json)
         ("guix" ,guix)
         ,@(package-propagated-inputs guix)))
      (arguments
       `(#:modules ((guix build guile-build-system)
                    (guix build utils)
                    (srfi srfi-26)
                    (ice-9 popen)
                    (ice-9 rdelim))
                   #:phases
                   (modify-phases %standard-phases
                     (replace 'unpack
                       (lambda* (#:key inputs #:allow-other-keys)
                         (for-each (lambda (file)
                                     (copy-file (string-append (assoc-ref inputs "source") "/" file)
                                                (string-append "./" file)))
                                   '("zabbix_guix" "zabbix.scm"))))
                     (add-after 'unpack 'setenv
                       (lambda _
                         (setenv "PATH"
                                 (string-append (assoc-ref %build-inputs "bash") "/bin" ":"
                                                (getenv "PATH")))))
                     (add-after 'install 'install-script
                       (lambda* (#:key inputs outputs #:allow-other-keys)
                         (let* ((out (assoc-ref outputs "out"))
                                (bin (string-append out "/bin"))
                                (zabbix-guix (string-append bin "/zabbix_guix"))
                                (guile (assoc-ref inputs "guile"))
                                (guile-bin (string-append guile "/bin/guile"))
                                (git (assoc-ref inputs "guile-git"))
                                (bs (assoc-ref inputs "guile-bytestructures"))
                                (gcrypt (assoc-ref inputs "guile-gcrypt"))
                                (json (assoc-ref inputs "guile-json"))
                                (guix (assoc-ref inputs "guix"))
                                (deps (list out gcrypt json bs git guix))
                                (effective
                                 (read-line
                                  (open-pipe* OPEN_READ guile-bin
                                              "-c" "(display (effective-version))")))
                                (path   (string-join
                                         (map (cut string-append <>
                                                   "/share/guile/site/"
                                                   effective)
                                              deps)
                                         ":"))
                                (gopath (string-join
                                         (map (cut string-append <>
                                                   "/lib/guile/" effective
                                                   "/site-ccache")
                                              deps)
                                         ":")))
                           (mkdir-p bin)
                           (substitute* "./zabbix_guix"
                             (("\\$\\(which guile\\)") guile-bin))
                           (install-file "./zabbix_guix" bin)
                           (chmod zabbix-guix #o555)
                           (wrap-program zabbix-guix
                             `("GUILE_LOAD_PATH" ":" prefix (,path))
                             `("GUILE_LOAD_COMPILED_PATH" ":" prefix (,gopath)))
                           #t))))))
      (home-page "https://anongit.duckdns.org/guix/zabbix-guix")
      (description "This package provides a Guile script to monitor Guix
channels difference.")
      (synopsis "Monitor Guix in Zabbix")
      (license license:gpl3+))))
