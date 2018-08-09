;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (wigust packages monitoring)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages django)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages image)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages))

(define-public zabbix
  (package
    (name "zabbix")
    (version "3.4.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/zabbix/ZABBIX%20Latest%20Stable/" version
             "/zabbix-" version ".tar.gz"))
       (sha256
        (base32
         "0qxgf6hx7ibhjmxd2sxizkjc8df4c9d31wz5hhql409ws98qf173"))))
    (arguments
     `(#:configure-flags
       (list "--enable-agent"
             (string-append "--with-iconv="
                            (assoc-ref %build-inputs "libiconv"))
             (string-append "--with-libpcre="
                            (assoc-ref %build-inputs "pcre"))
             "--enable-server"
             "--with-postgresql"
             (string-append "--with-libevent="
                            (assoc-ref %build-inputs "libevent"))
             "--with-net-snmp"
             (string-append "--with-gnutls="
                            (assoc-ref %build-inputs "gnutls"))
             "--with-libcurl")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-frontend
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((php (string-append (assoc-ref outputs "out")
                                        "/share/zabbix/php"))
                    (front-end-conf (string-append php "/conf"))
                    (etc (string-append php "/etc")))
               (mkdir-p php)
               (copy-recursively "./frontends/php" php)
               (rename-file front-end-conf
                            (string-append front-end-conf "-example"))
               (symlink "/etc/zabbix" front-end-conf)))))))
    (build-system gnu-build-system)
    (inputs
     `(("libiconv" ,libiconv)
       ("pcre" ,pcre)
       ;; Server
       ("curl" ,curl)
       ("libevent" ,libevent)
       ("gnutls" ,gnutls)
       ("postgresql" ,postgresql)
       ("zlib" ,zlib)
       ("net-snmp" ,net-snmp)
       ("curl" ,curl)))
    (home-page "https://www.zabbix.com/")
    (synopsis "Distributed monitoring solution (client-side agent)")
    (description "This package provides a distributed monitoring
solution (client-side agent)")
    (license license:gpl2)))

(define-public autopostgresqlbackup
  (package
    (name "autopostgresqlbackup")
    (version "1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/autopgsqlbackup"
                                  "/AutoPostgreSQLBackup/AutoPostgreSQLBackup-"
                                  version "/autopostgresqlbackup.sh." version))
              (file-name (string-append name "-" version ".sh"))
              (sha256
               (base32
                "15a3fw7prd64nq742apsxj0mqiib3nzfp4bdls8dck4n8yjxzl89"))))
    (build-system trivial-build-system)
    (inputs
     `(("bash" ,bash)
       ("gzip" ,gzip)
       ("postgresql" ,postgresql)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((autopostgresqlbackup-file (string-append ,name ".sh")))
           ;; Copy source
           (copy-file (assoc-ref %build-inputs "source")
                      autopostgresqlbackup-file)
           ;; Patch
           (substitute* autopostgresqlbackup-file
             (("#!/bin/bash")
              ;; Substitute shebang and add conditional load configuration.
              (string-append "#!" (assoc-ref %build-inputs "bash") "/bin/bash

# Load the autopostgresqlbackup configuration file.
if [ -f \"$HOME/.config/autopostgresqlbackup.conf\" ]
then
    . \"$HOME/.autopostgresqlbackup.conf\"
elif [ -f \"/etc/autopostgresqlbackup.conf\" ]
then
    . \"/etc/autopostgresqlbackup.conf\"
fi
"))
             (("^PATH=.*$" str)
              ;; Make sure Guix system and user profiles present in ‘PATH’.
              (string-append str
                             ":/run/current-system/profile/bin"
                             ":$HOME/.guix-profile/bin"))
             (("psql")
              ;; Make sure ‘psql’ program will be found in right place.
              (string-append (assoc-ref %build-inputs "postgresql")
                             "/bin/psql")))
           (let-syntax ((comment-line-with-variable
                         (syntax-rules ()
                           ((_ file (var ...))
                            (substitute* file
                              (((string-append "^" var "=.*$") str)
                               (format #f "# ~a" str)) ...)))))
             (comment-line-with-variable autopostgresqlbackup-file
                                         ("USERNAME" "DBHOST" "DBNAMES"
                                          "BACKUPDIR" "MAILCONTENT"
                                          "MAXATTSIZE" "MAILADDR" "MDBNAMES"
                                          "DBEXCLUDE" "CREATE_DATABASE"
                                          "SEPDIR" "DOWEEKLY" "COMP"
                                          "POSTBACKUP")))
           ;; Install
           (install-file autopostgresqlbackup-file
                         (string-append %output "/bin"))
           ;; Documentation
           (with-output-to-file "autopostgresqlbackup.8"
             (lambda ()
               (display
                (string-append
                 ".\\\" Copyright (C), 2018  Oleg Pykhalov <go.wigust@gmail.com>
.\\\" You may distribute this file under the terms of the GNU Free
.\\\" Documentation License.
.TH " ,name " 8 " ,version "
.SH NAME
" ,name " \\- backup all of your PostgreSQL databases daily,
weekly, and monthly
.SH SYNOPSIS
.B " ,name "

.SH DESCRIPTION
" ,name " is a shell script (usually executed from a cron job)
designed to provide a fully automated tool to make periodic backups of
PostgreSQL databases.

" ,name " can be configured by editing some options in file
$HOME/.config/" ,name ".conf or /etc/" ,name ".conf.
.SH FILES
/etc/" ,name ".conf $HOME/.config/" ,name ".conf
"))))
           (invoke (string-append (assoc-ref %build-inputs "gzip")
                                  "/bin/gzip")
                   "autopostgresqlbackup.8")
           (install-file "autopostgresqlbackup.8.gz"
                         (string-append %output "/share/man/man8"))
           #t))))
    (home-page "http://projects.frozenpc.net/autopostgresqlbackup/")
    (synopsis "Automated tool to make periodic backups of PostgreSQL databases")
    (description "autopostgresqlbackup is a shell script (usually executed
from a cron job) designed to provide a fully automated tool to make periodic
backups of PostgreSQL databases.  autopostgresqlbackup extract databases into
flat files in a daily, weekly or monthly basis.")
    (license license:gpl2+)))
