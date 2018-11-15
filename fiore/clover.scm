;; GuixSD configuration file for the desktop machine.
;; Copyright © 2017, 2018 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

(use-modules (gnu)
             (gnu system nss)
             (linux-nonfree)
             (guix store))

(use-service-modules ssh desktop xorg cups networking version-control mail)

(use-package-modules admin android bash bootloaders certs cups
databases dns file fonts fontutils freedesktop gnome gnupg linux mail
ncurses networking ratpoison readline rsync pulseaudio screen ssh tmux
version-control virtualization wget xdisorg xorg zile)

(define 20-intel.conf "
# Fix tearing on intel
# https://wiki.archlinux.org/index.php/Intel_Graphics
# https://github.com/8p8c/my-guix/blob/master/config.scm
Section \"Device\"
   Identifier  \"Intel Graphics\"
   Driver      \"intel\"
   Option      \"TearFree\" \"true\"
EndSection
")

(define %guix-daemon-config
  (guix-configuration
   (substitute-urls '("https://guix.duckdns.org"
                      "https://berlin.guixsd.org"
                      "https://mirror.hydra.gnu.org"
                      "https://hydra.gnu.org"))
   (max-silent-time 7200)
   (timeout (* 4 max-silent-time))
   (extra-options '("--max-jobs=4" "--cores=2"
                    "--cache-failures"
                    "--gc-keep-outputs=yes"
		    "--gc-keep-derivations=yes"))))

(define %custom-desktop-services
  (modify-services %desktop-services
    (special-files-service-type config => `(("/bin/sh"
                                             ,(file-append
                                               bash "/bin/sh"))
                                            ("/usr/bin/env"
                                             ,(file-append
                                               coreutils "/bin/env"))))
    (guix-service-type config => %guix-daemon-config)
    (slim-service-type config => (slim-configuration
                                  (inherit config)
                                  (startx
                                   (xorg-start-command
                                    #:configuration-file
                                    (xorg-configuration-file
                                     #:extra-config (list 20-intel.conf))))
                                  (auto-login? #f)
                                  (default-user "natsu")))))

(operating-system
  (host-name "clover")
  (timezone "Europe/Moscow")
  (locale "en_US.utf8")

  (bootloader (bootloader-configuration (bootloader grub-bootloader) (target "/dev/sda")))

  (kernel linux-nonfree)
  (firmware (list firmware-non-free))

  (file-systems (cons (file-system
                        (device "clover-root")
                        (title 'label)
                        (mount-point "/")
                        (type "ext4"))
                      %base-file-systems))

  (users (cons (user-account
                (name "natsu")
                (uid 1000)
                (comment "Oleg Pykhalov")
                (group "users")
                (supplementary-groups '("wheel"
                                        "audio" "video"))
                (home-directory "/home/natsu"))
               %base-user-accounts))
    (packages
     (cons*
      desktop-file-utils
      dovecot
      gvfs
      setxkbmap   ; Keyboard layout
      wmctrl      ; `ewmctrl'
      xclip       ; X clipboard CLI
      xdg-utils
      xdotool     ; Mouse and keyboard automation
      xorg-server ; `xephyr'
      xrandr      ; Change screen resolution
      xrdb
      xset
      xsetroot
      xterm       ; $TERM
      xwininfo    ; X Window information
      ;; For helm-stumpwm-commands and stumpish
      rlwrap
      xprop
      xhost

      adb       ; For Replicant (Android distribution) control
      cups      ; Printer
      ethtool   ; wol (wake on lan)
      file      ; Information about file from magic
      gnupg
      iptables
      knot
      lm-sensors      ; `sensors'
      ncurses
      nss-certs ; for https
      openssh   ; `scp'
      pinentry  ; Password typing for Gnupg
      qemu
      rsync
      strace
      tcpdump
      tmux
      tree      ; List files as a tree
      wget
      xkill
      zile

      adwaita-icon-theme
      hicolor-icon-theme
      font-awesome
      font-dejavu
      font-liberation
      font-misc-misc  ; for `xterm'
      font-wqy-zenhei ; Chinese, Japanese, Korean
      fontconfig      ; `fc-cache -f'

      alsa-utils
      pavucontrol ; Pulseaudio control GUI
      pulseaudio

      %base-packages))

  (services (cons* (service openssh-service-type
                            (openssh-configuration
			     (permit-root-login #t)
                             (port-number 22)))
                   (service cups-service-type
                            (cups-configuration
                             (web-interface? #t)
                             (extensions
                              (list cups-filters hplip-minimal))))
                   (service guix-publish-service-type
                            (guix-publish-configuration
                             (host "0.0.0.0")))
                   (service git-daemon-service-type
                            (git-daemon-configuration
                             (user-path "")))
                   (dovecot-service
                    #:config (dovecot-configuration
                              (mail-location
                               (string-append
                                "maildir:~/Maildir:INBOX=~/Maildir/INBOX:"
                                "LAYOUT=fs"))
                              (disable-plaintext-auth? #f)
                              (listen '("127.0.0.1"))))
                   %custom-desktop-services))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
