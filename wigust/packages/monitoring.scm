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
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
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
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls))

(define-public net-snmp
  (package
    (name "net-snmp")
    (version "5.7.3")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "mirror://sourceforge/net-snmp/"
                                  name ".zip"))
              (sha256
               (base32
                "0gkss3zclm23zwpqfhddca8278id7pk6qx1mydpimdrrcndwgpz8"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    (inputs
     `(("file" ,file)
       ("perl" ,perl)
       ("openssl" ,openssl)))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'autoreconf
           (lambda _
             (invoke "autoreconf" "-vfi"))))))
    (home-page "http://net-snmp.sourceforge.net/")
    (synopsis "Clients and server for the SNMP network monitoring protocol")
    (description "This package provides clients and server for the SNMP
network monitoring protocol")
    (license license:bsd-3)))

(define-public zabbix
  (package
    (name "zabbix")
    (version "3.4.11")
    (source
     (origin
       (method url-fetch)
       ;; https://netcologne.dl.sourceforge.net/project/zabbix/ZABBIX%20Latest%20Stable/3.4.11/zabbix-3.4.11.tar.gz
       (uri (string-append "https://netcologne.dl.sourceforge.net\
/project/zabbix/ZABBIX%20Latest%20Stable/" version
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
                            (assoc-ref %build-inputs "libevent")))
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
               (rename-file front-end-conf (string-append front-end-conf "-example"))
               (symlink "/etc/zabbix" front-end-conf)))))))
    (build-system gnu-build-system)
    (inputs
     `(("libiconv" ,libiconv)
       ("pcre" ,pcre)
       ;; Server
       ("curl" ,curl)
       ("libevent" ,libevent)
       ("openssl" ,openssl)
       ("postgresql" ,postgresql)
       ("zlib" ,zlib)))
    (home-page "https://www.zabbix.com/")
    (synopsis "Distributed monitoring solution (client-side agent)")
    (description "This package provides a distributed monitoring
solution (client-side agent)")
    (license license:gpl2)))
