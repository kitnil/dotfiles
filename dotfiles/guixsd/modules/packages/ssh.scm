(define-module (packages ssh)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(define-public openssh-8.6p1
  (package
   (name "openssh")
   (version "8.6p1")
   (source (origin
             (method url-fetch)
             (uri (string-append "mirror://openbsd/OpenSSH/portable/"
                                 "openssh-" version ".tar.gz"))
             (patches (search-patches "openssh-hurd.patch"))
             (sha256
              (base32
               "1bnpivgk98h2f9afpp88jv6g9ps83vnpxd031n2jqxi12vdf9rn3"))))
   (build-system gnu-build-system)
   (native-inputs `(("groff" ,groff)
                    ("pkg-config" ,pkg-config)))
   (inputs `(("libedit" ,libedit)
             ("openssl" ,openssl)
             ;; ,@(if (hurd-target?)
             ;;     '()
             ;;     `(("pam" ,linux-pam)))
             ("pam" ,linux-pam)
             ("mit-krb5" ,mit-krb5)
             ("zlib" ,zlib)
             ("xauth" ,xauth)))        ; for 'ssh -X' and 'ssh -Y'
   (arguments
    `(#:test-target "tests"
      ;; Otherwise, the test scripts try to use a nonexistent directory and
      ;; fail.
      #:make-flags '("REGRESSTMP=\"$${BUILDDIR}/regress\"")
      #:configure-flags  `("--sysconfdir=/etc/ssh"

                           ;; Default value of 'PATH' used by sshd.
                          "--with-default-path=/run/current-system/profile/bin"

                          ;; configure needs to find krb5-config.
                          ,(string-append "--with-kerberos5="
                                          (assoc-ref %build-inputs "mit-krb5")
                                          "/bin")

                          ;; libedit is needed for sftp completion.
                          "--with-libedit"

                          ;; Enable PAM support in sshd.
                          ;; ,,@(if (hurd-target?)
                          ;;      '()
                          ;;      '("--with-pam"))
                          "--with-pam"

                          ;; "make install" runs "install -s" by default,
                          ;; which doesn't work for cross-compiled binaries
                          ;; because it invokes 'strip' instead of
                          ;; 'TRIPLET-strip'.  Work around this.
                          ,,@(if (%current-target-system)
                                 '("--disable-strip")
                                 '()))

      #:phases
      (modify-phases %standard-phases
        (add-after 'configure 'reset-/var/empty
         (lambda* (#:key outputs #:allow-other-keys)
           (let ((out (assoc-ref outputs "out")))
             (substitute* "Makefile"
               (("PRIVSEP_PATH=/var/empty")
                (string-append "PRIVSEP_PATH=" out "/var/empty")))
             #t)))
        (add-before 'check 'patch-tests
         (lambda _
           (substitute* "regress/test-exec.sh"
             (("/bin/sh") (which "sh")))

           ;; Remove 't-exec' regress target which requires user 'sshd'.
           (substitute* (list "Makefile"
                              "regress/Makefile")
             (("^(tests:.*) t-exec(.*)" all pre post)
              (string-append pre post)))
           #t))
        (replace 'install
         (lambda* (#:key outputs (make-flags '()) #:allow-other-keys)
           ;; Install without host keys and system configuration files.
           (apply invoke "make" "install-nosysconf" make-flags)
           (install-file "contrib/ssh-copy-id"
                         (string-append (assoc-ref outputs "out")
                                        "/bin/"))
           (chmod (string-append (assoc-ref outputs "out")
                                 "/bin/ssh-copy-id") #o555)
           (install-file "contrib/ssh-copy-id.1"
                         (string-append (assoc-ref outputs "out")
                                        "/share/man/man1/"))
           #t)))))
   (synopsis "Client and server for the secure shell (ssh) protocol")
   (description
    "The SSH2 protocol implemented in OpenSSH is standardised by the
IETF secsh working group and is specified in several RFCs and drafts.
It is composed of three layered components:

The transport layer provides algorithm negotiation and a key exchange.
The key exchange includes server authentication and results in a
cryptographically secured connection: it provides integrity, confidentiality
and optional compression.

The user authentication layer uses the established connection and relies on
the services provided by the transport layer.  It provides several mechanisms
for user authentication.  These include traditional password authentication
as well as public-key or host-based authentication mechanisms.

The connection layer multiplexes many different concurrent channels over the
authenticated connection and allows tunneling of login sessions and
TCP-forwarding.  It provides a flow control service for these channels.
Additionally, various channel-specific options can be negotiated.")
   (license (license:non-copyleft "file://LICENSE"
                               "See LICENSE in the distribution."))
   (home-page "https://www.openssh.com/")))
