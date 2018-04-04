;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (fiore majordomo)
  #:use-module (gnu)
  #:use-module (gnu bootloader u-boot)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module ((guix store) #:select (%store-prefix))
  #:use-module (gnu services shepherd)
  #:use-module (gnu services ssh)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages nvi)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:export (installation-os))

;;; Commentary:
;;;
;;; This module provides an 'operating-system' definition for use on images
;;; for USB sticks etc., for the installation of the GNU system.
;;;
;;; Code:


(define (log-to-info)
  "Return a script that spawns the Info reader on the right section of the
manual."
  (program-file "log-to-info"
                #~(begin
                    ;; 'gunzip' is needed to decompress the doc.
                    (setenv "PATH" (string-append #$gzip "/bin"))

                    (execl (string-append #$info-reader "/bin/info") "info"
                           "-d" "/run/current-system/profile/share/info"
                           "-f" (string-append #$guix "/share/info/guix.info")
                           "-n" "System Installation"))))

(define %backing-directory
  ;; Sub-directory used as the backing store for copy-on-write.
  "/tmp/guix-inst")

(define (make-cow-store target)
  "Return a gexp that makes the store copy-on-write, using TARGET as the
backing store.  This is useful when TARGET is on a hard disk, whereas the
current store is on a RAM disk."

  (define (set-store-permissions directory)
    ;; Set the right perms on DIRECTORY to use it as the store.
    #~(begin
        (chown #$directory 0 30000)             ;use the fixed 'guixbuild' GID
        (chmod #$directory #o1775)))

  #~(begin
      ;; Bind-mount TARGET's /tmp in case we need space to build things.
      (let ((tmpdir (string-append #$target "/tmp")))
        (mkdir-p tmpdir)
        (mount tmpdir "/tmp" "none" MS_BIND))

      (let* ((rw-dir (string-append target #$%backing-directory))
             (work-dir (string-append rw-dir "/../.overlayfs-workdir")))
        (mkdir-p rw-dir)
        (mkdir-p work-dir)
        (mkdir-p "/.rw-store")
        #$(set-store-permissions #~rw-dir)
        #$(set-store-permissions "/.rw-store")

        ;; Mount the overlay, then atomically make it the store.
        (mount "none" "/.rw-store" "overlay" 0
               (string-append "lowerdir=" #$(%store-prefix) ","
                              "upperdir=" rw-dir ","
                              "workdir=" work-dir))
        (mount "/.rw-store" #$(%store-prefix) "" MS_MOVE)
        (rmdir "/.rw-store"))))

(define cow-store-service-type
  (shepherd-service-type
   'cow-store
   (lambda _
     (shepherd-service
      (requirement '(root-file-system user-processes))
      (provision '(cow-store))
      (documentation
       "Make the store copy-on-write, with writes going to \
the given target.")

      ;; This is meant to be explicitly started by the user.
      (auto-start? #f)

      (start #~(case-lambda
                 ((target)
                  #$(make-cow-store #~target)
                  target)
                 (else
                  ;; Do nothing, and mark the service as stopped.
                  #f)))
      (stop #~(lambda (target)
                ;; Delete the temporary directory, but leave everything
                ;; mounted as there may still be processes using it since
                ;; 'user-processes' doesn't depend on us.  The 'user-file-systems'
                ;; service will unmount TARGET eventually.
                (delete-file-recursively
                 (string-append target #$%backing-directory))))))))

(define (cow-store-service)
  "Return a service that makes the store copy-on-write, such that writes go to
the user's target storage device rather than on the RAM disk."
  ;; See <http://bugs.gnu.org/18061> for the initial report.
  (service cow-store-service-type 'mooooh!))

(define %nscd-minimal-caches
  ;; Minimal in-memory caching policy for nscd.
  (list (nscd-cache (database 'hosts)
                    (positive-time-to-live (* 3600 12))

                    ;; Do not cache lookup failures at all since they are
                    ;; quite likely (for instance when someone tries to ping a
                    ;; host before networking is functional.)
                    (negative-time-to-live 0)

                    (persistent? #f)
                    (max-database-size (* 5 (expt 2 20)))))) ;5 MiB

(define %installation-services
  ;; List of services of the installation system.
  (let ((motd (plain-file "motd" "
\x1b[1;37mWelcome to the installation of the Guix System Distribution!\x1b[0m

\x1b[2mThere is NO WARRANTY, to the extent permitted by law.  In particular, you may
LOSE ALL YOUR DATA as a side effect of the installation process.  Furthermore,
it is 'beta' software, so it may contain bugs.

You have been warned.  Thanks for being so brave.\x1b[0m
")))
    (define (normal-tty tty)
      (mingetty-service (mingetty-configuration (tty tty)
                                                (auto-login "root")
                                                (login-pause? #t))))

    (define bare-bones-os
      (load "/home/natsu/src/guix/gnu/system/examples/bare-bones.tmpl"))

    (list (service virtual-terminal-service-type)

          (mingetty-service (mingetty-configuration
                             (tty "tty1")
                             (auto-login "root")))

          (login-service (login-configuration
                          (motd motd)))

          ;; Documentation.  The manual is in UTF-8, but
          ;; 'console-font-service' sets up Unicode support and loads a font
          ;; with all the useful glyphs like em dash and quotation marks.
          (mingetty-service (mingetty-configuration
                             (tty "tty2")
                             (auto-login "natsu")
                             (login-program (log-to-info))))

          ;; A bunch of 'root' ttys.
          (normal-tty "tty3")
          (normal-tty "tty4")
          (normal-tty "tty5")
          (normal-tty "tty6")

          ;; The usual services.
          (syslog-service)

          ;; The build daemon.  Register the hydra.gnu.org key as trusted.
          ;; This allows the installation process to use substitutes by
          ;; default.
          (guix-service (guix-configuration (authorize-key? #t)))

          ;; Start udev so that useful device nodes are available.
          ;; Use device-mapper rules for cryptsetup & co; enable the CRDA for
          ;; regulations-compliant WiFi access.
          (udev-service #:rules (list lvm2 crda))

          ;; Add the 'cow-store' service, which users have to start manually
          ;; since it takes the installation directory as an argument.
          (cow-store-service)

          ;; Install Unicode support and a suitable font.  Use a font that
          ;; doesn't have more than 256 glyphs so that we can use colors with
          ;; varying brightness levels (see note in setfont(8)).
          (service console-font-service-type
                   (map (lambda (tty)
                          (cons tty "lat9u-16"))
                        '("tty1" "tty2" "tty3" "tty4" "tty5" "tty6")))

          ;; To facilitate copy/paste.
          (gpm-service)

          ;; Add an SSH server to facilitate remote installs.
          (service openssh-service-type
                   (openssh-configuration
                    (port-number 22)
                    (permit-root-login #t)
                    ;; The root account is passwordless, so make sure
                    ;; a password is set before allowing logins.
                    (allow-empty-passwords? #f)
                    (password-authentication? #t)

                    ;; Don't start it upfront.
                    (%auto-start? #f)))

          ;; Since this is running on a USB stick with a overlayfs as the root
          ;; file system, use an appropriate cache configuration.
          (nscd-service (nscd-configuration
                         (caches %nscd-minimal-caches)))

          ;; Having /bin/sh is a good idea.  In particular it allows Tramp
          ;; connections to this system to work.
          (service special-files-service-type
                   `(("/bin/sh" ,(file-append (canonical-package bash)
                                              "/bin/sh"))))

          ;; Keep a reference to BARE-BONES-OS to make sure it can be
          ;; installed without downloading/building anything.  Also keep the
          ;; things needed by 'profile-derivation' to minimize the amount of
          ;; download.
          (service gc-root-service-type
                   (list bare-bones-os
                         glibc-utf8-locales
                         texinfo
                         (canonical-package guile-2.2))))))

(define %issue
  ;; Greeting.
  "
\x1b[1;37mThis is an installation image of the GNU system.  Welcome.\x1b[0m

\x1b[1;33mUse Alt-F2 for documentation.\x1b[0m
")

(define installation-os
  ;; The operating system used on installation images for USB sticks etc.
  (operating-system
    (host-name "gnu")
    (timezone "Europe/Moscow")
    (locale "en_US.utf8")
    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (target "/dev/sda")))
    (file-systems
     ;; Note: the disk image build code overrides this root file system with
     ;; the appropriate one.
     (cons* (file-system
              (mount-point "/")
              (device "GuixSD_image")
              (title 'label)
              (type "ext4"))

            ;; Make /tmp a tmpfs instead of keeping the overlayfs.  This
            ;; originally was used for unionfs because FUSE creates
            ;; '.fuse_hiddenXYZ' files for each open file, and this confuses
            ;; Guix's test suite, for instance (see
            ;; <http://bugs.gnu.org/23056>).  We keep this for overlayfs to be
            ;; on the safe side.
            (file-system
              (mount-point "/tmp")
              (device "none")
              (title 'device)
              (type "tmpfs")
              (check? #f))

            ;; XXX: This should be %BASE-FILE-SYSTEMS but we don't need
            ;; elogind's cgroup file systems.
            (list %pseudo-terminal-file-system
                  %shared-memory-file-system
                  %immutable-store)))

    (users (list (user-account
                  (name "natsu")
                  (group "users")
                  (supplementary-groups '("wheel")) ; allow use of sudo
                  (password "")
                  (comment "Oleg Pykhalov")
                  (home-directory "/home/natsu"))))

    (issue %issue)
    (services %installation-services)

    ;; We don't need setuid programs, except for 'passwd', which can be handy
    ;; if one is to allow remote SSH login to the machine being installed.
    (setuid-programs (list (file-append shadow "/bin/passwd")))

    (pam-services
     ;; Explicitly allow for empty passwords.
     (base-pam-services #:allow-empty-passwords? #t))

    (packages (cons* (canonical-package glibc) ;for 'tzselect' & co.
                     parted gptfdisk ddrescue
                     grub                  ;mostly so xrefs to its manual work
                     cryptsetup
                     mdadm
                     dosfstools         ;mkfs.fat, for the UEFI boot partition
                     btrfs-progs
                     openssh    ;we already have sshd, having ssh/scp can help
                     wireless-tools iw wpa-supplicant-minimal iproute
                     ;; XXX: We used to have GNU fdisk here, but as of version
                     ;; 2.0.0a, that pulls Guile 1.8, which takes unreasonable
                     ;; space; furthermore util-linux's fdisk is already
                     ;; available here, so we keep that.
                     bash-completion
                     nvi                          ;:wq!
                     %base-packages))))

;; Return the default os here so 'guix system' can consume it directly.
installation-os

;;; install.scm ends here
