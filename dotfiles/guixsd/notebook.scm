;; This is an operating system configuration template
;; for a "desktop" setup without full-blown desktop
;; environments.

(use-modules (gnu)
	     (gnu system nss)
	     (nongnu packages linux)
	     (nongnu system linux-initrd)

	     (services openvpn)

             (ice-9 match)

	     (services nix)
	     (srfi srfi-1)
	     (srfi srfi-26))

(use-service-modules desktop networking ssh nix)

(use-package-modules bootloaders certs vpn wm terminals xfce linux package-management admin fonts)

(use-service-modules desktop dbus networking xorg)

(operating-system
  (host-name "notebook")
  (timezone "Europe/Moscow")
  (locale "en_US.utf8")

  (initrd microcode-initrd)
  (kernel linux-5.15)
  (firmware (cons* linux-firmware %base-firmware))

  ;; Use the UEFI variant of GRUB with the EFI System
  ;; Partition mounted on /boot/efi.
  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets '("/boot/efi"))))

    (mapped-devices (list (mapped-device
                           (source "vg0")
                           (targets '("vg0-guixroot"))
                           (type lvm-device-mapping))))

  ;; Assume the target root file system is labelled "my-root",
  ;; and the EFI System Partition has UUID 1234-ABCD.
  (file-systems (append
                 (list (file-system
                         (device "/dev/mapper/vg0-guixroot")
                         (mount-point "/")
                         (dependencies mapped-devices)
                         (type "ext4"))
                       (file-system
                         (device (uuid "0A05-C141" 'fat))
                         (mount-point "/boot/efi")
                         (type "vfat")))
                 %base-file-systems))

  (groups (cons* (user-group (name "nixbld")
                             (id 30100))
                 %base-groups))

  (users (cons (user-account
                (name "oleg")
                (group "users")
                (supplementary-groups '("wheel" "netdev"
                                        "audio" "video")))
	       (append ((lambda* (count #:key
                                        (group "nixbld")
                                        (first-uid 30101)
                                        (shadow shadow))
				 (unfold (cut > <> count)
					 (lambda (n)
                                           (user-account
                                            (name (format #f "nixbld~a" n))
                                            (system? #t)
                                            (uid (+ first-uid n -1))
                                            (group group)

                                            ;; guix-daemon expects GROUP to be listed as a
                                            ;; supplementary group too:
                                            ;; <http://lists.gnu.org/archive/html/bug-guix/2013-01/msg00239.html>.
                                            (supplementary-groups (list group "kvm"))

                                            (comment (format #f "Nix Build User ~a" n))
                                            (home-directory "/var/empty")
                                            (shell (file-append shadow "/sbin/nologin"))))
					 1+
					 1))
                        9)
                       %base-user-accounts)))

  ;; Add a bunch of window managers; we can choose one at
  ;; the log-in screen with F1.
  (packages (append (list
                     ;; window managers
                     ;;sway dmenu
                     ;; terminal emulator
                     ;; for HTTPS access
lvm2
		     openvpn
sway
alacritty
;ratpoison
;xfce4-terminal
nss-certs
nix)
                    %base-packages))

  (sudoers-file (plain-file "sudoers"
                            (string-join `("root ALL=(ALL) ALL"
                                           "%wheel ALL=(ALL) ALL"
                                           "oleg ALL=(ALL) NOPASSWD:ALL")
                                         "\n")))

  ;; Use the "desktop" services, which include the X11
  ;; log-in service, networking with NetworkManager, and more.
  (services (append (list
                     (service wpa-supplicant-service-type)    ;needed by NetworkManager
                     (service network-manager-service-type)
                     (service openssh-service-type)
		     (service openvpn-service-type
			      (openvpn-configuration
			       (name "wugi.info")
			       (config (plain-file "openvpn.conf"
						   "\
client
proto udp
dev tapvpn1
ca /etc/openvpn/ca.crt
cert /etc/openvpn/client.crt
key /etc/openvpn/client.key
comp-lzo
persist-key
persist-tun
verb 3
nobind
ping 5
ping-restart 10
resolv-retry infinite
remote guix.wugi.info 1195
remote-random
route 141.80.181.40 255.255.255.255 192.168.25.2
"))))

                     #;(service openvpn-service-type
                              (openvpn-configuration
                               (name "majordomo.ru")
                               (auto-start? #f)
                               (config
                                (plain-file "openvpn.conf"
                                            "\
client
port 1194
proto udp
dev tapvpn
verb 2

# ns{1,2}-mr.intr
remote vpn-miran.majordomo.ru 1194 udp
cipher AES-256-GCM
data-ciphers AES-256-GCM

# remote vpn-dh.majordomo.ru
# remote 78.108.91.250 1194 udp

# ctrl3.ihc-ru.net is an IHC Puppet server
route 46.254.22.60 255.255.255.255

#<connection>
#remote 78.108.87.250 1194 udp
#float
#</connection>

# remote 78.108.80.230 1194 udp

remote-cert-tls server
<ca>
-----BEGIN CERTIFICATE-----
MIIFIzCCAwugAwIBAgIUDBKyvFnWiPrKYM8PUc/ldQmdhOswDQYJKoZIhvcNAQEL
BQAwGTEXMBUGA1UEAxMOTWFqb3Jkb21vIFJvb3QwHhcNMjIwNjIyMDkzNDQxWhcN
MzIwNjE5MDkzNTA1WjAZMRcwFQYDVQQDEw5NYWpvcmRvbW8gUm9vdDCCAiIwDQYJ
KoZIhvcNAQEBBQADggIPADCCAgoCggIBAJeF5Z5rLLbCIDRuqZuWhQNVADTFmAiO
14SkhC2w0UvTiAJkyfzXhw4aW3TtT8pit3BEvhQSzKP7iJyOpmJoOmQRS7Q4WXmx
7HfWMaYtJ1T8MmRWR/b0pxwidIdaLjlk0C5x+VYuSVata2D2D/IU8ljphYxY3eNF
5eo49X3HqYMsweeYiSQRzdJF9lXn7mTxMXuOmKnHr3QRQz3F+hzEBwunZp09pD0B
8T9TMkujVUPXQGbNQhsO7bXlGvCqc/VzTxmms14t3gd2BtqNGQtPDS6NDIRPN2XP
DUvPFcqx7cBYw92qVpEiw98Yjfk3aUOA+jYzMGulmtyB5WdgYQXPyW0l84ll+4vj
W9HUsMSaCl9uwkNFSoXH2CCPYHctDbiPEi6T4X8dQzMl0cPjemUrFmxvfubI+bvw
c/FlHxIYMl7q8CLqOohgd2+MBZX5PQpeRRETErgOGHTKKyg0decHuYpa3T22SV//
/iGsy9+ghsOpDViWPxkDpkuSFPcqIaAU4riV71kH2mQmFyJYlGiI4ZdTA0QrrGhB
rXnLfPZAIabh0B9UfMwRvJ6wt6K+m/bTFk98XU2ssVjDp9PL0tbbAOeSiy0yquqb
HFOK1gtV3Mu2t1AwpAbC5Wnn4CATWgvPsAhIBiG6715jja4/Hgp+EzzvzNIjU/Aw
L1RPZTPWKWhDAgMBAAGjYzBhMA4GA1UdDwEB/wQEAwIBBjAPBgNVHRMBAf8EBTAD
AQH/MB0GA1UdDgQWBBTgRtctMpFWEHXLwZDYSfp51IYUNDAfBgNVHSMEGDAWgBTg
RtctMpFWEHXLwZDYSfp51IYUNDANBgkqhkiG9w0BAQsFAAOCAgEAjDH3A++Q1NvN
WEnYlu8I77nAG1dcHMGvFjCfIyZ2YTPT6IUK2DqBSytNmr7bKWSIZ4sOvJoFvpZG
KIWDOXvNDb1eSRz+/tMSa6We5jxRUH2GIK7Rocb4gYDY8cWMs8pcNRaQ3xERejZk
kIpsm365C+jR3kq7dG7O4adPLI247gDmFkQBaOADPxnvatBw05MmMKel2V/d3JAk
1ZyPgKAg6Nvlbn0MBWU+O2F55a5d/mwoxwbLJp4JE1kbRlTLnFq6ekeSMIMueG3U
rOQg9sjvrQQaPNbws9uBm8HxeZHyc0hkrTCEsB+Pt86MTcppCPdvGtQ7PyypG/Kc
H2d4QlJDGlBhnqUNzzh+cxgm5zN7R+jgLL2ogDfyP31G/DMuR+rU99x/Ia10FL+N
fcK/4hZU/ko/wZqSn/PbYQkrzS2tiy+TWnWv3qkDwv5XNd4XFYg9uwlqufmDEb2t
GqPWl7ltJogbabTJsx54hmUasBMWxCLBn1DZuX21A4oN/LkdvJntKi9Fd7Bt8U6K
/IINHsLnng4G7wfBYVJjNxXg3fXBR60KgfNMrE3FvqBpIco512DN4zm+Kw0q79x3
sWLCFVcsiETRPTMSdzrdb4MJR4up9wQWQFoCW/DgvXCwcpppKaYVfmDKgBrJNLlr
ZBYq6PjqbFeXvrMBPWwl83Aln0qr33A=
-----END CERTIFICATE-----
-----BEGIN CERTIFICATE-----
MIIEMzCCAhugAwIBAgIUB4f151JlH/9iA5oagMR76PGO63gwDQYJKoZIhvcNAQEL
BQAwGTEXMBUGA1UEAxMOTWFqb3Jkb21vIFJvb3QwHhcNMjIwNjIyMDkzNzE1WhcN
MjcwNjIxMDkzNzQ1WjApMScwJQYDVQQDEx5NYWpvcmRvbW8gSW50ZXJtZWRpYXRl
IE9wZW5WUE4wggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQDZ1OnWA+QZ
NO6eXinPgD+niYzknNjWP4ZTyv3abc/ormt/nRfSrjr0FRr3q0sza3xINSrO9bY3
pNLsp4FuNEfS3zY1LGSKbR1oYf7FytzwCyVuv11LnYYTTtI/dCd2AFP+OU/8NuFu
XwiD0020mUiefuTf0ahklRncaKxH0wl8q1ON3kABMNopuJ0R4p37w7zCI4RUl9c5
Nhe7R7N6xTPNBShCLEyIU+o3L0/k2O8neEza9Css7M+Tu04+EMOp61gjZ1zKELvr
IMlR6/pbyXp4Rz6ydcW5relWEHju5qkg6s0byrXlCGg+vWmeZteWykgwroCafkSb
OAsIv21K+ocRAgMBAAGjYzBhMA4GA1UdDwEB/wQEAwIBBjAPBgNVHRMBAf8EBTAD
AQH/MB0GA1UdDgQWBBSAy3/LOiK6VkadQI+2GchgzPWdwjAfBgNVHSMEGDAWgBTg
RtctMpFWEHXLwZDYSfp51IYUNDANBgkqhkiG9w0BAQsFAAOCAgEABt06s/BQqIvM
Cz2lmTkcVU58eNJEVBrv4esZs1Uzkz7mnj5wKXVQFtBsbyva22CNZ3eZWJaxKogi
+niUrQ7TBxgfc7kY8/voyEewo0hKSdJMjimwoKaQ2pX17YYYIBJtdYagNodMQKGL
2WeUAZp3gPfB0DJcDTvx9X90/B4G0IeQxloqhh4F1WnU51ORakcFHSoYSsTI741U
SjL06w7bV7kVb1kaCB7S27aHe1tV71ME224mvVSWUhH2xGw4yoHy2THACcASJQUs
8uE2OViVHig0J3N2ILdPr17/8cAPERG6OsLYe/JSRqPFMtHQMd5oUxlgfCp/wDAC
3O7vi4+6kUv2GxZ4533f5fyrNkQX8uzP64Sb+b6tbQlhMVI5g4OFGfuzrDtgKzTG
4QL1Snrj9gC+3cZ3enbsx/b4dqyAKdBFYiUU0n4zlomxwYYyEUxrA7FjqKwNH1nk
a9f4ygSJSeOu6JrmBlAu1RFfqmeBre1n4whd9debkEju6d8it4x1MNVHRT4CwEKk
+SaE2HOidLBl25nh4L/rYngYjIUAGGn02ecuH/SjXVeUhGZG2w822GE0XVV+8GOt
qvoZwDADOh3d8dyEW4srKjfjE4TMdsBAORbNL2LrBiegOJOm8yKFOcOIXvmzIoL9
+vvpTT74WoGiBcGR9/gtw5+XB9vm+XA=
-----END CERTIFICATE-----
</ca>
auth SHA1
script-security 3
auth-nocache
auth-retry nointeract
ping 10
ping-restart 15
auth-user-pass /etc/openvpn/login.conf
remote-random
"))))

		     nix-service)

                    (list ;; (screen-locker-service slock)
                          (udisks-service)
                          (service upower-service-type)
                          (service accountsservice-service-type)
                          (service colord-service-type)
                          (geoclue-service)
                          (service polkit-service-type)
                          (elogind-service)
                          (dbus-service)
                          (service ntp-service-type))

                    (modify-services
                     (modify-services %base-services
                                      (guix-service-type config =>
                                                         (guix-configuration
                                                          (authorized-keys (append (list (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/guix.wugi.info.pub")
                                                                                         (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/vm1.wugi.info.pub")
                                                                                         (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/vm2.wugi.info.pub")
                                                                                         (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/vm3.wugi.info.pub")
                                                                                         (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/jenkins.intr.pub")
                                                                                         (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/spb.pub")
                                                                                         (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/mirror.brielmaier.net.pub")
                                                                                         (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/substitutes.nonguix.org.pub"))
                                                                                   %default-authorized-guix-keys))
                                                          (substitute-urls '("https://ci.guix.gnu.org"
                                                                             "https://guix.wugi.info"
                                                                             "https://substitutes.nonguix.org"))))
                                      ;; (sysctl-service-type _ =>
                                      ;;                      (sysctl-configuration
                                      ;;                       (settings (append '(("net.ipv4.ip_forward" . "1")
                                      ;;                                           ("net.ipv4.conf.all.rp_filter" . "0")
                                      ;;                                           ("net.ipv4.conf.default.rp_filter" . "0"))
                                      ;;                                         %default-sysctl-settings))))
                                      )
                     (console-font-service-type
                      configuration =>
                      (map
                       (match-lambda
                         (("tty1" . f)
                          `("tty1" . ,(file-append font-terminus
                                                   "/share/consolefonts/ter-132n")))
                         (("tty2" . f)
                          `("tty2" . ,(file-append font-terminus
                                                   "/share/consolefonts/ter-132n")))
                         (("tty3" . f)
                          `("tty3" . ,(file-append font-terminus
                                                   "/share/consolefonts/ter-132n")))
                         (("tty4" . f)
                          `("tty4" . ,(file-append font-terminus
                                                   "/share/consolefonts/ter-132n")))
                         (("tty5" . f)
                          `("tty5" . ,(file-append font-terminus
                                                   "/share/consolefonts/ter-132n")))
                         (("tty6" . f)
                          `("tty6" . ,(file-append font-terminus
                                                  "/share/consolefonts/ter-132n")))
                         ((tty . font) `(,tty . ,font)))
                       configuration)))))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
