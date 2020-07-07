;; This is an operating system configuration for a VM image.
;; Modify it as you see fit and instantiate the changes by running:
;;
;;   guix system reconfigure /etc/config.scm
;;

(use-modules (gnu) (guix) (srfi srfi-1) (srfi srfi-26))
(use-service-modules desktop networking ssh xorg)
(use-package-modules admin base bootloaders certs package-management wget xorg zile)

(operating-system
  (host-name "guix.vm.wugi.info")
  (timezone "Europe/Moscow")
  (locale "en_US.utf8")

  ;; Below we assume /dev/vda is the VM's hard disk.
  ;; Adjust as needed.
  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (target "/dev/vda")
               (terminal-outputs '(console))))
  (file-systems (cons (file-system
                        (mount-point "/")
                        (device "/dev/vda1")
                        (type "ext4"))
                      %base-file-systems))

  (groups (cons* (user-group (name "nixbld")
                             (id 30100))
                 %base-groups))

  (users (cons* (user-account
                 (name "oleg")
                 (comment "Oleg Pykhalov")
                 (uid 1000)
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

  (sudoers-file (plain-file "sudoers" "\
root ALL=(ALL) ALL
%wheel ALL=(ALL) ALL
oleg ALL=(ALL) NOPASSWD:ALL\n"))

  (packages (append (list nss-certs wget zile)
                    %base-packages))

  (services
   (append (list (service openssh-service-type)
                 (static-networking-service "eth0" "78.108.82.157"
                                            #:netmask "255.255.254.0"
                                            #:gateway "78.108.83.254"
                                            #:name-servers '("8.8.8.8" "8.8.4.4"))
                 (extra-special-file "/usr/bin/env"
                                     (file-append coreutils "/bin/env")))
           %base-services)))
