;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021, 2022, 2023, 2024 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (services backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:export (restic-rest-configuration
            restic-rest-service-type

            restic-system-backup
            restic-guix-backup
            restic-win10-backup
            restic-win2022-backup
            restic-ntfsgames-backup

            restic-command))

;;; Commentary:
;;;
;;; This module provides a service definition for the backup service.
;;;
;;; Code:

(define-record-type* <restic-rest-configuration>
  restic-rest-configuration make-restic-rest-configuration
  restic-rest-configuration?
  (restic-rest     restic-rest-configuration-restic-rest     ;string
                   (default #f))
  (listen-address  restic-rest-configuration-listen-address  ;string
                   (default "0.0.0.0:8080"))
  (data-path       restic-rest-configuration-data-path       ;string
                   (default "/var/lib/restic"))
  (append-only?    restic-rest-configuration-append-only?    ;boolean
                   (default #f))
  (prometheus?     restic-rest-configuration-prometheus?     ;boolean
                   (default #f))
  (private-repos?  restic-rest-configuration-private-repos?  ;boolean
                   (default #f))
  (arguments       restic-rest-configuration-arguments       ;list of strings
                   (default '()))
  (user            restic-rest-configuration-user            ;string
                   (default "restic"))
  (group           restic-rest-configuration-group           ;string
                   (default "restic"))
  (authentication? restic-rest-configuration-authentication? ; boolean
                   (default #t)))

(define (restic-account config)
  (list (user-account
         (name "restic")
         (group "restic")
         (system? #t)
         (comment "Restic REST privilege separation user")
         (home-directory "/var/lib/restic"))
        (user-group
         (name "restic")
         (system? #t))))

(define (restic-rest-activation config)
  #~(begin
      (let* ((user (getpw "restic"))
             (home (passwd:dir user))
             (uid (passwd:uid user))
             (group (getgrnam "restic"))
             (gid (group:gid group))
             (data-path #$(restic-rest-configuration-data-path config)))
        (mkdir-p data-path)
        (chown data-path uid gid))))

(define (restic-rest-shepherd-service config)
  (list
   (shepherd-service
    (provision '(restic-rest))
    (documentation "Run Restic REST.")
    (requirement '(user-processes loopback))
    (start #~(make-forkexec-constructor
              (list #$(restic-rest-configuration-restic-rest config)
                    "--listen" #$(restic-rest-configuration-listen-address config)
                    "--path" #$(restic-rest-configuration-data-path config)
                    #$@(if (restic-rest-configuration-append-only? config)
                           '("--append-only")
                           '())
                    #$@(if (restic-rest-configuration-private-repos? config)
                           '("--private-repos")
                           '())
                    #$@(if (restic-rest-configuration-prometheus? config)
                           '("--prometheus")
                           '())
                    #$@(if (restic-rest-configuration-authentication? config)
                           '()
                           '("--no-auth"))
                    #$@(restic-rest-configuration-arguments config))
              #:log-file "/var/log/restic-rest.log"
              #:user #$(restic-rest-configuration-user config)
              #:group #$(restic-rest-configuration-group config)))
    (respawn? #f)
    (stop #~(make-kill-destructor)))))

(define restic-rest-service-type
  (service-type (name 'restic-rest)
                (extensions (list (service-extension account-service-type
                                                     restic-account)
                                  (service-extension shepherd-root-service-type
                                                     restic-rest-shepherd-service)
                                  (service-extension activation-service-type
                                                     restic-rest-activation)))
                (description "Run Restic REST.")))


;;;
;;; Restic
;;;

(define %root-directories
  '("/root/.cache"
    "/root/.guix-profile"
    "/root/.nix-profile"))

(define %user-directories
  '(".cache"
    ".guix-profile"
    ".nix-profile"
    "Downloads"
    "GNS3"
    "Videos"
    "vm"))

(define restic-binary
  (file-append restic "/bin/restic"))

(define virsh-binary
  (file-append libvirt "/bin/virsh"))

(define dd-binary
  (file-append coreutils "/bin/dd"))

(define curl-binary
  (file-append curl "/bin/curl"))

(define lvcreate-binary
  (file-append lvm2 "/sbin/lvcreate"))

(define lvremove-binary
  (file-append lvm2 "/sbin/lvremove"))

(define %user-home
  (passwd:dir (getpw "oleg")))

(define (restic-system-backup)
  (program-file
   "restic-backup-system"
   #~(begin
       (use-modules (ice-9 rdelim)
                    (srfi srfi-1))
       (let ((%backup-directories (list #$%user-home
                                        "/etc"
                                        "/root"
                                        "/var/lib/grafana"
                                        "/var/lib/crowdsec"
                                        "/var/lib/opensearch"
                                        "/var/lib/docker"
                                        "/var/lib/peertube"
                                        "/var/lib/peertube_assets"))
             (%exclude-directories
              (append '#$%root-directories
                      (map (lambda (directory)
                             (string-append #$%user-home "/" directory))
                           '#$%user-directories))))
         (setenv "RESTIC_PASSWORD"
                 (string-trim-right
                  (with-input-from-file "/etc/guix/secrets/restic"
                    read-string)))
         (display "Creating new Restic system snapshot\n")
         (zero?
          (apply system*
                 (append (list #$restic-binary "--no-cache"
                               "--repo" "/srv/backup/guixsd")
                         (fold (lambda (directory directories)
                                 (append (list "--exclude" directory) directories))
                               '() %exclude-directories)
                         (list "backup")
                         %backup-directories)))))))

(define* (restic-lv-backup vg lv
                           #:key (predicate #~(begin #t))
                           restic-repository
                           restic-password
                           (lvm2-snapshot-size "32G"))
  "Return a GEXP which defines a logical volume backup steps."
  (let* ((device (string-append "/dev/" vg "/" lv))
         (lvm2-snapshot-name (string-append lv "-snap"))
         (lvm2-snapshot-device (string-append device "-snap")))
    (program-file
     (string-append "restic-backup-" vg "-" lv)
     #~(begin
         (use-modules (ice-9 rdelim)
                      (srfi srfi-1))
         (if #$predicate
             (begin
               (format #t "Creating new Restic ~a snapshot~%" #$device)
               (setenv "RESTIC_PASSWORD" #$(restic-password))
               (zero?
                (system
                 (string-join
                  (append (list #$dd-binary
                                (string-append "if=" #$device)
                                "bs=4M" "status=none")
                          (list "|")
                          (list #$restic-binary "--no-cache"
                                "--repo" #$restic-repository
                                "backup" "--stdin" "--stdin-filename"
                                #$(string-append lv ".img")))))))
             (every identity
                    (list
                     (begin
                       (format #t "Creating new LVM ~a snapshot~%" #$device)
                       (system* #$lvcreate-binary
                                "--size" #$lvm2-snapshot-size
                                "--name" #$lvm2-snapshot-name
                                "--snapshot" #$device))
                     (begin
                       (format #t "Creating new Restic ~a snapshot~%" #$device)
                       (setenv "RESTIC_PASSWORD" #$(restic-password))
                       (zero?
                        (system
                         (string-join
                          (append (list #$dd-binary
                                        (string-append "if=" #$lvm2-snapshot-device)
                                        "bs=4M" "status=none")
                                  (list "|")
                                  (list #$restic-binary "--no-cache"
                                        "--repo" #$restic-repository
                                        "backup" "--tag" "snapshot"
                                        "--stdin" "--stdin-filename"
                                        #$(string-append lv ".img")))))))
                     (begin
                       (format #t "Delete LVM snapshot and save changes ~a snapshot~%"
                               #$lvm2-snapshot-device)
                       (zero?
                        (system* #$lvremove-binary "--yes"
                                 #$lvm2-snapshot-device))))))))))

(define (virtual-machine-shut-off? virtual-machine-name)
  #~(begin
      (use-modules (ice-9 popen)
                   (ice-9 rdelim))
      (let* ((port (open-pipe* OPEN_READ #$virsh-binary
                               "domstate" #$virtual-machine-name))
             (output (read-string port)))
        (close-port port)
        (string= (string-trim-right output #\newline)
                 "shut off"))))

(define (win10-password)
  #~(begin
      (use-modules (ice-9 rdelim))
      (string-trim-right
       (with-input-from-file "/etc/guix/secrets/windows"
         read-string))))

(define (restic-win10-backup)
  (restic-lv-backup "lvm1" "win10"
                    #:restic-repository "/srv/backup/win10"
                    #:restic-password win10-password
                    #:predicate (virtual-machine-shut-off? "win10")))

(define (win2022-password)
  #~(begin
      (use-modules (ice-9 rdelim))
      (string-trim-right
       (with-input-from-file "/etc/guix/secrets/windows"
         read-string))))

(define (restic-win2022-backup)
  (restic-lv-backup "lvm2" "win2022"
                    #:restic-repository "/srv/backup/win2022"
                    #:restic-password win2022-password
                    #:predicate (virtual-machine-shut-off? "win2022")))

(define (restic-ntfsgames-backup)
  (restic-lv-backup "lvm2" "ntfsgames"
                    #:restic-repository "/srv/backup/ntfsgames"
                    #:restic-password win10-password
                    #:predicate (virtual-machine-shut-off? "win10")))

(define (guix-password)
  #~(begin
      (use-modules (ice-9 rdelim))
      (string-trim-right
       (with-input-from-file "/etc/guix/secrets/guix"
         read-string))))

(define (restic-guix-backup)
  (restic-lv-backup "lvm2" "guix"
                    #:restic-repository "/srv/backup/guix"
                    #:restic-password guix-password
                    #:predicate (virtual-machine-shut-off? "guix")))

(define (restic-command)
  (program-file
   "restic-commands"
   #~(map (lambda (program)
            (zero? (system* program)))
          (list #$(restic-system-backup)
                #$(restic-guix-backup)
                #$(restic-win10-backup)
                #$(restic-win2022-backup)
                #$(restic-ntfsgames-backup)))))

;;; backup.scm ends here
