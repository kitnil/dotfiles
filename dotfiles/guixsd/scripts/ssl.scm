#!/run/current-system/profile/bin/guile \
--no-auto-compile -e (ssl) -s
!#

;;;; ssl --- Deploy ssl for DOMAINS keys to MACHINES
;;;; Copyright © 2020, 2021 Oleg Pykhalov <go.wigust@gmail.com>
;;;; Released under the GNU GPLv3 or any later version.

;; M-x compile
;; (set -ex; sudo -i /home/oleg/ssl.scm githunt.wugi.info; ssh vm1.wugi.info -- sudo ls -l /etc/letsencrypt/archive/githunt.wugi.info/privkey2.pem)

(define-module (ssl)
  #:export (main))

;;; Commentary:
;;;
;;; DESCRIPTION
;;;
;;; Code:

(define %dry-run?
  #f)

;; SSL certificates for domains which will be coppied to %machines.
(define %domains
  '("githunt.wugi.info"
    "homer.wugi.info"))

;; Machines available via OpenSSH.
(define %machines
  '("vm1.wugi.info" "vm2.wugi.info"))

(define %ssh-user
  "oleg")

(define %ssh-key
  "/home/oleg/.ssh/id_rsa")

(define %ssh-args
  '("-o" "UserKnownHostsFile=/dev/null"
    "-o" "StrictHostKeyChecking=no"))

(define (file-canonicalized file)
  (canonicalize-path (string-append (dirname file) "/" (readlink file))))

(define (system** . args)
  (if %dry-run?
      (begin (display (string-join args))
             (pk (list 'system* args))
             (newline))
      (begin (display (string-join args))
             (newline)
             (apply system* args))))

(define (ssh . args)
  (apply system** (append (list "ssh" "-i" %ssh-key) %ssh-args args)))

(define (main args)
  (for-each (lambda (domain)
              (let* ((live-directory (string-append "/etc/letsencrypt/live/" domain))
                     (fullchain-source (string-append live-directory "/fullchain.pem"))
                     (fullchain-destination (file-canonicalized fullchain-source))
                     (privkey-source (string-append live-directory "/privkey.pem"))
                     (privkey-destination (file-canonicalized privkey-source))
                     (archive (string-append domain ".tar"))
                     (host-archive (string-append "/tmp/" archive)))
                (system** "tar" "--verbose" "--create" "--file" host-archive
                          fullchain-destination privkey-destination)
                (for-each (lambda (machine)
                            (let ((ssh-destination (string-append %ssh-user "@" machine)))
                              (format #t "Deploy to ~a...~%" machine)
                              (apply system**
                                     (append (list "scp" "-v" "-i" %ssh-key)
                                             %ssh-args
                                             (list host-archive (string-append "oleg@" machine ":"))))
                              (ssh ssh-destination "--"
                                   "sudo" "tar" "--extract" "--file" archive "--directory=/" "--verbose")
                              (ssh ssh-destination "--" "rm" "-f" archive)
                              (ssh ssh-destination "--" "sudo" "mkdir" "-p" live-directory)
                              (ssh ssh-destination "--" "sudo" "ln" "-sf" fullchain-destination fullchain-source)
                              (ssh ssh-destination "--" "sudo" "ln" "-sf" privkey-destination privkey-source)
                              (ssh ssh-destination "--" "sudo" "herd" "restart" "nginx")))
                          %machines)
                (system** "rm" "-f" host-archive)))
            %domains))

;;; ssl ends here
