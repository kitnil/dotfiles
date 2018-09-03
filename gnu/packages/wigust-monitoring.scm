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

(define-module (gnu packages wigust-monitoring)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system trivial)
  #:use-module (guix utils)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages django)
  #:use-module (gnu packages file)
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

(define-public net-snmp
  (package
    (name "net-snmp")
    (version "5.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/net-snmp/net-snmp/" version
                           "/net-snmp-" version ".tar.gz"))
       (sha256
        (base32
         "1w5l9w0sgi1zkzq8ww6kc6fzq7ljq59z2d9ks6bdq1vp7ihqkvqj"))
       (patches
        (map (lambda (file hash)
               (origin
                 (method url-fetch)
                 (uri (string-append
                       "https://git.alpinelinux.org\
/cgit/aports/plain/main/net-snmp/"
                       file "?id=f25d3fb08341b60b6ccef424399f060dfcf3f1a5"))
                 (sha256
                  (base32
                   hash))))
             '("CVE-2015-5621.patch"
               "fix-Makefile-PL.patch"
               "fix-includes.patch"
               "netsnmp-swinst-crash.patch"
               "remove-U64-typedef.patch")
             '("0mg2mlfb45fnv7m1k9wckrqjfizipyvrl1q4dn1r0zc774mm7zjc"
               "1pd85sy04n76q1ri3l33f0zpnnw76nd5mcny2j39ilzp76bjfik5"
               "0zpkbb6k366qpq4dax5wknwprhwnhighcp402mlm7950d39zfa3m"
               "0gh164wy6zfiwiszh58fsvr25k0ns14r3099664qykgpmickkqid"
               "0jcpcpgx4z9k1w0x6km0132n67qc29mz6cialwfjm02l76q2yk5n")))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (inputs
     `(("file" ,file)
       ("perl" ,perl)
       ("openssl" ,openssl)))
    (arguments
     `(#:tests? #f
       #:configure-flags
       (list "--with-default-snmp-version=3"
             "--with-sys-location=Unknown"
             "--with-sys-contact=root@unknown"
             "--with-logfile=/var/log/net-snmpd.log"
             "--with-persistent-directory=/var/lib/net-snmp"
             (string-append "--with-openssl="
                            (assoc-ref %build-inputs "openssl"))
             "--with-mnttab=/proc/mounts")
       #:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "INSTALLSITEARCH=" out
                              "/lib/perl5/site_perl/" ,(package-version perl)
                              "/x86_64-linux-thread-multi")
               (string-append"INSTALLSITEMAN3DIR=" out "/share/man/man3")))

       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'autoreconf
           (lambda _
             (invoke "autoreconf" "-vfi"))))))
    (home-page "http://net-snmp.sourceforge.net/")
    (synopsis "Clients and server for the SNMP network monitoring protocol")
    (description "The Simple Network Management Protocol (SNMP) provides a
framework for the exchange of management information between agents (servers)
and clients.

The Net-SNMP applications are a collection of command line clients for issuing
SNMP requests to agents.")
    (license license:bsd-3)))

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
