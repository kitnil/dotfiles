;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (services ssh)
  #:use-module (gnu)
  #:export (%ssh-users
            %ssh-hosts-file-hosts))

;;; Commentary:
;;;
;;; This module provides a service definition for the ssh service.
;;;
;;; Code:

(define %ssh-users
  (list (user-account
         (name "majordomo-ssh-tunnel")
         (uid 30011)
         (group "users")
         (comment "SSH forwarding privilege separation user")
         (home-directory "/home/majordomo-ssh-tunnel"))
        (user-account
         (name "tail-ssh-tunnel")
         (uid 30015)
         (group "users")
         (comment "SSH forwarding privilege separation user")
         (home-directory "/home/tail-ssh-tunnel"))
        (user-account
         (name "spb-zabbix-ssh-tunnel")
         (uid 30020)
         (group "users")
         (comment "SSH forwarding privilege separation user")
         (home-directory "/home/spb-zabbix-ssh-tunnel"))
        (user-account
         (name "oracle-ssh-tunnel")
         (uid 30021)
         (group "users")
         (comment "SSH forwarding privilege separation user")
         (home-directory "/home/oracle-ssh-tunnel"))
        (user-account
         (name "vm1-ssh-tunnel")
         (uid 30022)
         (group "users")
         (comment "SSH forwarding privilege separation user")
         (home-directory "/home/vm1-ssh-tunnel"))))

(define %ssh-hosts-file-hosts
  '("back.wugi.info"))

;;; ssh.scm ends here
