;; GuixSD configuration file for the desktop machine.
;; Copyright © 2017 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

(use-modules (gnu)
             (guix packages)
             (guix build-system gnu)
             (srfi srfi-1)
             (ice-9 popen)
             (ice-9 rdelim))

(use-service-modules ssh desktop xorg cups version-control mail networking
                     shepherd rsync web spice)

(use-package-modules bash bootloaders certs linux android version-control
                     cups emacs xorg)


;;;
;;; Firewall service
;;;

(define start-firewall
  #~(let ((iptables
           (lambda (str)
             (zero? (system (string-join `(,#$(file-append iptables
                                                           "/sbin/iptables")
                                           ,str) " "))))))
      (format #t "Install iptables rules.~%")
      (and
       ;; Rules to throttle malicious SSH connection attempts.  This will
       ;; allow at most 3 connections per minute from any host, and will block
       ;; the host for another minute if this rate is exceeded.  Taken from
       ;; <http://www.la-samhna.de/library/brutessh.html#3>.
       (iptables "-A INPUT -p tcp --dport 22 \
-m state --state NEW -m recent --set --name SSH -j ACCEPT")
       (iptables "-A INPUT -p tcp --dport 22 \
-m recent --update --seconds 60 --hitcount 4 --rttl \
--name SSH -j LOG --log-prefix SSH_brute_force")
       (iptables "-A INPUT -p tcp --dport 22 \
-m recent --update --seconds 60 --hitcount 4 --rttl \
--name SSH -j DROP")

       ;; Rules to accept web traffic only on private network.
       (iptables "-A INPUT -p tcp --dport 80 -s 192.168.0.0/16 -j ACCEPT")
       (iptables "-A INPUT -p tcp --dport 80 -s 127.0.0.0/8 -j ACCEPT")
       (iptables "-A INPUT -p tcp --dport 80 -j DROP")

       ;; Rules to throttle HTTP connection redirections.  Taken from
       ;; <https://www.opennet.ru/tips/2999_iptables_block_tor.shtml>.
       (iptables "-A INPUT -p tcp --sport 443 --tcp-flags RST RST -j DROP")
       (iptables "-A INPUT -p tcp --sport 80 -m string \
--string \"Location: http://warning.rt.ru\" --algo bm -j DROP")
       (iptables "-A INPUT -p tcp --sport 80 -m string \
--string \"Location: http://promo.nw.rt.ru\" --algo bm -j DROP"))))

(define firewall-service
  ;; The "firewall".  Make it a Shepherd service because as an activation
  ;; script it might run too early, before the Netfilter modules can be
  ;; loaded for some reason.
  (simple-service 'firewall shepherd-root-service-type
                  (list
                   (shepherd-service
                    (provision '(firewall))
                    (requirement '())
                    (start #~(lambda _
                               #$start-firewall))
                    (respawn? #f)
                    (stop #~(lambda _
                              (zero?
                               (system* #$(file-append iptables
                                                       "/sbin/iptables")
                                        "-F"))))))))


;;;
;;; guix-daemon
;;;

(define %guix-daemon-config
  (guix-configuration
   ;; Import keys
   ;; $ wget https://git.savannah.gnu.org/cgit/guix/maintenance.git/plain/hydra/keys/guix/berlin.guixsd.org-export.pub
   ;; # “guix archive --authorize < berlin.guixsd.org-export.pub”
   (substitute-urls '("https://berlin.guixsd.org"
                      "https://mirror.hydra.gnu.org" "https://hydra.gnu.org"))
   ;; (authorized-keys '())
   (max-silent-time 7200)
   (timeout (* 4 max-silent-time))

   (extra-options '("--max-jobs=6" "--cores=3"))))


;;;
;;; Desktop services
;;;

(define 20-intel.conf "
# Fix tearing on intel
# https://wiki.archlinux.org/index.php/Intel_Graphics
# https://github.com/8p8c/my-guix/blob/master/config.scm
Section \"Device\"
   Identifier  \"Intel Graphics\"
   Driver      \"intel\"
   Option      \"AccelMethod\"  \"sna\"
   Option      \"SwapbuffersWait\" \"true\"
   Option      \"TearFree\" \"true\"
EndSection
")

(define %custom-desktop-services
  ;; Desktop service provides:
  ;;
  ;; Disable NETWORK-MANAGER-SERVICE-TYPE
  ;; Fix tearing on Intel video card 20-INTEL.CONF
  ;; Files /bin/sh and /usr/bin/env
  ;;
  ;; Inspired by https://lists.gnu.org/archive/html/help-guix/2016-01/msg00064.html
  (modify-services %desktop-services
    (guix-service-type config => %guix-daemon-config)
    (special-files-service-type config => `(("/bin/sh"
                                             ,(file-append
                                               bash "/bin/sh"))
                                            ("/usr/bin/env"
                                             ,(file-append
                                               coreutils "/bin/env"))))
    (slim-service-type config => (slim-configuration
                                  (inherit config)
                                  (startx
                                   (xorg-start-command
                                    #:configuration-file
                                    (xorg-configuration-file
                                     #:extra-config (list 20-intel.conf))))
                                  (auto-login? #f)
                                  (default-user "natsu")))))


;;;
;;; NGINX
;;;

(define %file-share-configuration-nginx
  (nginx-configuration
   (server-blocks
    (list (nginx-server-configuration
           (server-name '("www.magnolia.local"))
           (root "/srv/share")
           (https-port #f)
           (ssl-certificate #f)
           (ssl-certificate-key #f))))))

(define guix-publish-nginx-service
  (simple-service 'guix-publish-nginx nginx-service-type
   (list (nginx-server-configuration
          (server-name '("guix.magnolia.local"))
          (locations (list (nginx-location-configuration
                            (uri "/")
                            (body '("proxy_pass http://localhost:3000;")))))
          (https-port #f)
          (ssl-certificate #f)
          (ssl-certificate-key #f)))))

(define %cgit-configuration-nginx-body
    (list "fastcgi_param SCRIPT_FILENAME $document_root/lib/cgit/cgit.cgi;"
          "fastcgi_param PATH_INFO $uri;"
          "fastcgi_param QUERY_STRING $args;"
          "fastcgi_param HTTP_HOST $server_name;"
          "fastcgi_pass 127.0.0.1:9000;"))

(define %cgit-configuration-nginx
  (list (nginx-server-configuration
         (root cgit)
         (server-name '("cgit.magnolia.local"))
         (locations (list
                     (nginx-location-configuration
                      (uri "@cgit")
                      (body %cgit-configuration-nginx-body))))
         (try-files (list "$uri" "@cgit"))
         (https-port #f)
         (ssl-certificate #f)
         (ssl-certificate-key #f))))


;;;
;;; Support functions
;;;

(define (cartesian-product . lists)
  (fold-right (lambda (xs ys)
                (append-map (lambda (x)
                              (map (lambda (y)
                                     (cons x y))
                                   ys))
                            xs))
              '(())
              lists))


;;;
;;; /etc/hosts
;;;

(define (prefix-local-host-aliases prefix host-name domain)
  (string-join (map (lambda (x)
                      (string-append (string-join x " ") "." host-name domain))
                    (cartesian-product '("127.0.0.1" "::1") prefix))
               "\n"))


;;;
;;; Packages
;;;

(define emacs-athena
  ;; GTK+ could kill emacs --daemon,
  ;; see <https://bugzilla.gnome.org/show_bug.cgi?id=85715>.
  (package
    (inherit emacs)
    (name "emacs-athena")
    (synopsis "The extensible, customizable, self-documenting text
editor with athena toolkit" )
    (build-system gnu-build-system)
    (inputs `(("libxaw" ,libxaw)
              ,@(alist-delete "gtk+" (package-inputs emacs))))
    (arguments
     `(#:configure-flags '("--with-x-toolkit=athena")
                         ,@(package-arguments emacs)))))

(define (spec->packages spec)
  (call-with-values (lambda ()
                      (specification->package+output spec)) list))

(define %user-packages
  (map spec->packages
       (list
        "setxkbmap" ; Keyboard layout
        "xclip" ; X clipboard CLI
        "xrdb"
        "xset"
        "xsetroot"
        "xterm" ; $TERM
        "xorg-server" ; `xephyr'
        "wmctrl" ; `ewmctrl'
        "xwininfo" ; X Window information
        "xdg-utils"
        "xdotool" ; Mouse and keyboard automation
        "gvfs"
        "glib:bin"
        "desktop-file-utils"
        "xdg-utils"

        "emacs-znc"
        "znc"

        ;; For helm-stumpwm-commands and stumpish
        "rlwrap"
        "xprop"

        "translate-shell" ; Translation in CLI and Emacs

        "git" ; Version control
        "git:send-email"

        "gnu-c-manual" ; C language documentation
        "adb" ; For Replicant (Android distribution) control

        "sbcl" ; For StumpWM.  See <https://stumpwm.github.io/>.

        "gcc-toolchain" ; For Emacs `semantic-mode'
        "cflow"         ; C program call map
        "global"        ; Source tagging

        "screen" ; Terminal multiplexer

        "kodi-cli" ; Remote control Kodi

        "openssh" ; `scp'
        "nss-certs" ; for https

        "file" ; Information about file from magic
        "htop" ; Pretty `top'
        "ncdu" ; TUI `du'
        "netcat" ; TCP
        "python-pygments" ; Colorize output
        "tree" ; List files as a tree
        "cloc" ; Count code
        "unzip"

        "lm-sensors" ; `sensors'

        ;; Spelling
        "aspell"
        "aspell-dict-en"
        "aspell-dict-ru"

        "python-clf" ; Interface to <https://commandlinefu.com/>

        "graphviz" ; `dot'

        "adwaita-icon-theme"
        "font-dejavu"
        "font-liberation"
        "font-awesome"
        "font-wqy-zenhei" ; Chinese, Japanese, Korean
        "fontconfig" ; `fc-cache -f'
        "font-misc-misc" ; for `xterm'
        "ratpoison"
        "redshift"

        "feh" ; Image viewer
        "mpv" ; Video and audio player
        "ffmpeg" ; Video, audio, images, gif conversion
        "imagemagick" ; Pipe to `display'
        "obs" ; OBS Studio

        "icecat" ; Web browser

        "isync" ; Sync IMAP
        "notmuch" ; Mail indexer based on Xapian

        "qemu" ;; Encryption and signing
        "gnupg"
        "pinentry" ; Password typing for Gnupg

        "password-store" ; Password management

        "recutils" ; Filter records like in `guix --search'

        "pavucontrol" ; Pulseaudio control GUI
        "pulsemixer" ; Pulseaudio control CLI
        "alsa-utils"

        "transmission" ; Bittorrent

        ;; $EDITOR
        "emacs-aggressive-indent"  ; Auto indent minor mode
        "emacs-company"            ; Complition framework
        "emacs-company-quickhelp"  ; Help pages for Company
        "emacs-debbugs"            ; <https://debbugs.gnu.org/> interface
        "emacs-debpaste"           ; Front end to <https://paste.debian.net/>
        "emacs-elfeed"             ; RSS reader
        "emacs-engine-mode-autoload" ; Define searches on websites
        "emacs-erc-hl-nicks"       ; for ERC
        "emacs-eval-in-repl"       ; Evaluate to different Repls
        "emacs-ewmctrl"            ; Control X windows from Emacs
        "emacs-ggtags"             ; Front end to GNU Global
        "emacs-gitpatch"           ; Send patches
        "emacs-guix"               ; Guix interface
        "emacs-helm"               ; Narrowing framework
        "emacs-helm-firefox"       ; Search for bookmarks in Icecat
        "emacs-helm-make"          ; Front end to `make'
        "emacs-helm-pass"          ; Front end to password-store
        "magit"                    ; Emacs interface for Git
        "emacs-git-gutter"
        "emacs-helm-projectile"    ; Helm interface for Projectile
        "emacs-highlight-stages"   ; Highlight code stages
        "emacs-markdown-mode"      ; Commonmark major mode
        "emacs-org"                ; Org
        "emacs-multiple-cursors"   ; Multi cursor
        "emacs-nix-mode"           ; Nix language mode
        "emacs-org-mind-map"       ; General mind maps from Org files
        "emacs-projectile"         ; Project functions
        "emacs-slime"              ; Sbcl repl
        "emacs-smartparens"        ; Structured editing
        "strace"
        "emacs-move-text"
        "shellcheck"
        "emacs-strace-mode-special" ; Colorize `strace' logs
        "emacs-transmission"       ; Front end to transmission-daemon
        "emacs-transpose-frame"    ; M-x transpose-frame
        "emacs-use-package"        ; Lazy configuration
        "emacs-w3m"                ; Front end to w3m command line web browser
        "emacs-which-key"          ; Key bindings help
        "emacs-yasnippet"          ; Snippets
        "emacs-yasnippet-snippets" ; Collection of snippets
        "emacs-flycheck"           ; Syntax checker
        "emacs-rainbow-delimiters" ; Prettify parentheses
        "geiser"                   ; Scheme bridge

        "haunt"            ; Guile static site generator
        "guile-commonmark" ; Commonmark for Guile

        "gwl"              ; Guix workflow management

        "stow"             ; Dotfiles management
        "make"             ; GNU Make
        "ghc-pandoc"       ; Convert Markdown

        ;; Downloaders.
        "youtube-dl"   ; Video and music from websites
        "wget")))


;;;
;;; Operating system.
;;;

(define %system-magnolia
  (operating-system
    (host-name "magnolia")
    (timezone "Europe/Moscow")
    (locale "en_US.utf8")

    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (target "/boot/efi")))

    (file-systems (cons* (file-system
                           (device "magnolia-root")
                           (title 'label)
                           (mount-point "/")
                           (type "ext4"))
                         (file-system
                           (device "/dev/sda1")
                           (mount-point "/boot/efi")
                           (type "vfat"))
                         (file-system
                           (device "magnolia-data")
                           (title 'label)
                           (mount-point "/srv")
                           (type "ext4"))
                         (file-system
                           (device "tmpfs")
                           (mount-point "/tmp")
                           (type "tmpfs")
                           (flags '(no-dev))
                           (options "mode=1777,size=16G")
                           (needed-for-boot? #t)
                           (check? #f))
                         %base-file-systems))

    (groups (cons
             (user-group (name "adbusers"))
             %base-groups))

    (users (cons (user-account
                  (name "natsu")
                  (uid 1000)
                  (comment "Oleg Pykhalov")
                  (group "users")
                  (supplementary-groups '("wheel" "audio" "video" "lpadmin" "lp"
                                          "adbusers"))
                  (home-directory "/home/natsu"))
                 %base-user-accounts))

    (hosts-file
     ;; Create a /etc/hosts file with aliases for "localhost"
     ;; and "mymachine", as well as for Facebook servers.
     (plain-file "hosts"
                 (string-append (local-host-aliases host-name)
                                (prefix-local-host-aliases
                                 '("cgit" "guix" "www") host-name ".local")
                                %facebook-host-aliases)))

    (packages (cons emacs-athena
                    (append %user-packages %base-packages)))

    (services (cons* firewall-service

                     (service openssh-service-type)

                     (service cups-service-type
                              (cups-configuration
                               (web-interface? #t) ; LibreJS could block JS
                               (extensions (list cups-filters hplip))))

                     (dovecot-service
                      #:config (dovecot-configuration
                                (listen '("127.0.0.1"))
                                (disable-plaintext-auth? #f)
                                (mail-location
                                 (string-append "maildir:~/Maildir"
                                                ":INBOX=~/Maildir/INBOX"
                                                ":LAYOUT=fs"))))

                     (service guix-publish-service-type
                              (guix-publish-configuration
                               (host "0.0.0.0") (port 3000)))

                     (service git-daemon-service-type
                              (git-daemon-configuration (user-path "")))

                     (service rsync-service-type)

                     (service fcgiwrap-service-type)

                     (tor-service (local-file "torrc"))

                     (service nginx-service-type
                              %file-share-configuration-nginx)

                     (service cgit-service-type
                              (cgit-configuration
                               (nginx %cgit-configuration-nginx)))

                     guix-publish-nginx-service

                     (simple-service 'adb udev-service-type
                                     (list android-udev-rules))

                     %custom-desktop-services))

    ;; Allow resolution of '.local' host names with mDNS.
    (name-service-switch %mdns-host-lookup-nss)))

%system-magnolia

;;; system-magnolia.scm ends here
