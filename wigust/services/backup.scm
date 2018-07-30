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

(define-module (wigust services backup)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages admin)
  #:use-module (wigust packages monitoring)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:export (autopostgresqlbackup-configuration))

(define (uglify-field-name field-name)
  (string-upcase (string-delete #\? (case field-name
                                      ((separate-directory?) "sepdir")
                                      ((host) "dbhost")
                                      ((databases) "dbnames")
                                      ((backup-directory) "backupdir")
                                      ((mail-content) "mailcontent")
                                      ((max-attribute-size) "maxattsize")
                                      ((mail-address) "mailaddr")
                                      ((monthly-backup) "mdbnames")
                                      ((exclude-databases) "dbexclude")
                                      ((create-database?) "create_database")
                                      ((weekly-backup) "doweekly")
                                      ((compression-type) "comp")
                                      ((pre-backup) "prebackup")
                                      ((post-backup) "postbackup")
                                      (else (symbol->string field-name))))))

(define (serialize-field field-name val)
  (format #t "~a=\"~a\"\n" (uglify-field-name field-name) val))

(define (serialize-boolean field-name val)
  (serialize-field field-name (if val "yes" "no")))

(define (serialize-integer field-name val)
  (serialize-field field-name (number->string val)))

(define (serialize-list field-name val)
  (if (null? val) "" (serialize-field field-name (string-join val))))

(define (serialize-string field-name val)
  (if (and (string? val) (string=? val ""))
      ""
      (serialize-field field-name val)))

(define-configuration autopostgresqlbackup-configuration
  (username
   (string "postgresql")
   "Specifies the user name or ID that is used when running psql program.")
  (host
   (string "localhost")
   "Host name or IP address of PostgreSQL server")
  (databases
   (list '("all"))
   "List of DBNAMES for Daily/Weekly Backup")
  (backup-directory
   (string "/backups/pgsql")
   "Backup directory location")
  (mail-content
   (string "log")
   "What would you like to be mailed to you?
- log    : send only log file
- files  : send log file and sql files as attachments (see docs)
- stdout : will simply output the log to the screen if run manually.")
  (max-attribute-size
   (integer 4000)
   "Maximum allowed email size in k")
  (mail-address
   (string "root@localhost")
   "Email Address to send mail to")
  (monthly-backup
   (list '())
   "List of databases for monthly backups.")
  (exclude-databases
   (list '())
   "List of databases to exlucde if @code{dbnames} are set to all.")
  (create-database?
   (boolean #t)
   "Include @code{CREATE DATABASE} in backup.")
  (separate-directory?
   (boolean #t)
   "Separate backup directory and file for each DB")
  (weekly-backup
   (integer 6)
   "Which day do you want weekly backups (1 to 7 where 1 is Monday)")
  (compression-type
   (string "gzip")
   "Compression type")
  (pre-backup
   (string "")
   "Command to run before backups")
  (post-backup
   (string "")
   "Command to run after backups"))

;; (with-output-to-file "/tmp/local/autopostgresqlbackup.conf"
;;   (lambda ()
;;     (display
;;      (with-output-to-string
;;        (lambda ()
;;          (serialize-configuration (autopostgresqlbackup-configuration)
;;                                   autopostgresqlbackup-configuration-fields))))))
