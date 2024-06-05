;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022, 2023, 2024 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (services kubernetes)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (packages containers)
  #:use-module (packages kubernetes)
  #:use-module (srfi srfi-1)
  #:export (kubernetes-k3s-configuration
            kubernetes-k3s-configuration?
            kubernetes-k3s-service-type

            kubernetes-k3s-service

            containerd-service-type

            kubelet-configuration
            kubelet-configuration?
            kubelet-service-type

            edgecore-configuration
            edgecore-configuration?
            edgecore-service-type))

;;; Commentary:
;;;
;;; This module provides a service definition for the kubernetes service.
;;;
;;; Code:

(define-record-type* <kubernetes-k3s-configuration>
  kubernetes-k3s-configuration make-kubernetes-k3s-configuration
  kubernetes-k3s-configuration?
  (k3s       kubernetes-k3s-configuration-k3s       ;<package>
             (default k3s))
  (arguments kubernetes-k3s-configuration-arguments ;list of strings
             (default '()))
  (kubevirt? kubernetes-k3s-configuration-kubevirt? ;boolean
             (default #f))
  (cilium?   kubernetes-k3s-configuration-cilium?   ;boolean
             (default #f))
  (flux?     kubernetes-k3s-configuration-flux?     ;boolean
             (default #f))
  (log-file  kubernetes-k3s-configuration-log-file  ;string
             (default "/var/log/k3s.log"))
  (runtime   kubernetes-k3s-configuration-runtime   ;symbol
             (default 'docker))
  (server?   kubernetes-k3s-configuration-server?   ;boolean
             (default #f)))

(define (kubernetes-k3s-log-rotations config)
  (list (log-rotation
         (files (list (kubernetes-k3s-configuration-log-file config))))))

(define (cilium-requirements)
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (unless (zero? (system* #$(file-append util-linux+udev "/bin/mountpoint")
                                "--quiet" "/sys/fs/bpf"))
          (invoke #$(file-append util-linux+udev "/bin/mount")
                  "-o" "rw,nosuid,nodev,noexec,relatime,mode=700"
                  "-t" "bpf"
                  "none"
                  "/sys/fs/bpf")
          (invoke #$(file-append util-linux+udev "/bin/mount")
                  "--make-shared" "/sys/fs/bpf"))
        (invoke #$(file-append util-linux+udev "/bin/mount")
                "--make-shared" "/sys/fs/cgroup")
        (invoke #$(file-append util-linux+udev "/bin/mount")
                "--make-shared" "/"))))

(define %drbd-module
  "drbd9")

(define %hpvolumes
  "/dev/lvm1/hpvolumes")

(define %hpvolumes-mount-directory
  "/var/hpvolumes")

(define (hpvolumes-requirements)
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (unless (file-exists? #$%hpvolumes)
          (invoke (system* #$(file-append lvm2 "/sbin/lvchange")
                           #$%hpvolumes)))
        (unless (zero? (system* #$(file-append util-linux+udev "/bin/mountpoint")
                                "--quiet"
                                #$%hpvolumes-mount-directory))
          (invoke #$(file-append util-linux+udev "/bin/mount")
                  "/dev/lvm1/hpvolumes" "/var/hpvolumes")
          (invoke #$(file-append util-linux+udev "/bin/mount")
                  "--make-shared" "/var/hpvolumes")))))

(define (prlimit-requirements)
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (ice-9 rdelim)
                     (guix build utils))
        (let ((containerd-pid
               (string-trim
                (with-input-from-file "/run/containerd/containerd.pid"
                  read-string))))
          (invoke "prlimit" "--pid" containerd-pid "--nofile=1048576:1048576")
          (invoke "prlimit" "--pid" containerd-pid "--nproc=unlimited")))))

(define (drbd-requirements)
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (setenv "LINUX_MODULE_DIRECTORY"
                "/run/booted-system/kernel/lib/modules")
        (unless (file-exists? "/proc/drbd")
          (invoke #$(file-append kmod "/bin/modprobe")
                  #$%drbd-module)))))

(define %coredns-image
  "docker.io/coredns/coredns:1.7.1")

(define coredns-image-file
  "/nix/store/1crdy15nv25jpbvknrzyhg6khv9ikhl9-docker-image-coredns-coredns-1.7.1.tar")

(define %pause-image
  "docker.io/library/pause:latest")

(define pause-image-file
  "/nix/store/xjlwhyqjhx0j2sc41wfpsw1zvhn98vh5-docker-image-pause.tar.gz")

(define (containerd-load-image image-name image-file command)
  #~(begin
      (use-modules (ice-9 format)
                   (ice-9 popen)
                   (ice-9 rdelim)
                   (srfi srfi-1))
      (let* ((ctr #$(file-append containerd "/bin/ctr"))
             (port (open-pipe* OPEN_READ
                               ctr "--namespace" "k8s.io" "images" "list"))
             (output (read-string port)))
        (close-port port)
        (unless (any (lambda (line)
                       (string-prefix? #$image-name line))
                     (string-split (string-trim-right output #\newline)
                                   #\newline))
          (system
           (format #f
                   "~a ~a | ~a -n k8s.io image import --all-platforms -"
                   #$command
                   #$image-file
                   ctr))))))

(define (kubernetes-images)
  (with-imported-modules '((guix build utils))
    #~(begin
        #$(containerd-load-image %coredns-image coredns-image-file
                                 #$(file-append coreutils "/bin/cat"))
        #$(containerd-load-image %pause-image pause-image-file
                                 #$(file-append gzip "/bin/zcat")))))

(define (maintenance)
  (with-imported-modules '((guix build utils))
    #~(begin
        (mkdir-p "/var/lib/kubelet")
        (if (file-exists? "/var/lib/kubelet/.maintenance")
            (begin
              (display "File /var/lib/kubelet/.maintenance exists.")
              (invoke #$(file-append coreutils "/bin/sleep")
                       "infinity"))
            (invoke #$(file-append coreutils "/bin/touch")
                    "/var/lib/kubelet/.maintenance")))))

(define (etcd)
  (with-imported-modules '((guix build utils))
    #~(begin
        (mkdir-p "/var/lib/etcd"))))

(define (k3s-wrapper args)
  (program-file
   "k3s-wrapper"
   (with-imported-modules '((guix build utils))
     #~(begin
         #$(cilium-requirements)
         #$args))))

(define (kubernetes-k3s-shepherd-service config)
  (list
   (shepherd-service
    (provision '(kubernetes-k3s))
    (documentation "Run kubernetes-k3s.")
    (requirement (append '(networking containerd)
                         (case (kubernetes-k3s-configuration-runtime config)
                           ((docker) '(dockerd))
                           (else '()))))
    (start
     #~(make-forkexec-constructor
        (list
         #$(k3s-wrapper
            #~(execl #$(file-append (kubernetes-k3s-configuration-k3s config)
                                    "/bin/k3s")
                     "k3s"
                     #$(if (kubernetes-k3s-configuration-server? config)
                           "server"
                           "agent")
                     "--log" #$(kubernetes-k3s-configuration-log-file config)
                     #$@(kubernetes-k3s-configuration-arguments config))))))
    (respawn? #t) ;XXX: Fix race condition with Docker
    (stop #~(make-kill-destructor)))))

(define kubernetes-k3s-service-type
  (service-type
   (name 'kubernetes-k3s)
   (extensions
    (list (service-extension profile-service-type
                             (lambda (config)
                               (append (list k3s
                                             kubectl
                                             kubernetes-helm
                                             nerdctl
                                             k9s)
                                       (if (kubernetes-k3s-configuration-cilium? config)
                                           (list cilium)
                                           '())
                                       (if (kubernetes-k3s-configuration-flux? config)
                                           (list flux)
                                           '())
                                       (if (kubernetes-k3s-configuration-kubevirt? config)
                                           (list virtctl)
                                           '()))))
          (service-extension shepherd-root-service-type
                             kubernetes-k3s-shepherd-service)
          (service-extension rottlog-service-type
                             kubernetes-k3s-log-rotations)))
   (default-value '())
   (description "Run the kubernetes-k3s.")))


;;;
;;; containerd
;;;

;; Same as containerd from gnu/services/docker.scm in Guix official repository
;; but does not require Docker.

(define (containerd-shepherd-service config)
  (let ((containerd containerd))
    (list
     (shepherd-service
      (documentation "containerd daemon.")
      (provision '(containerd))
      (start #~(make-forkexec-constructor
                (list (string-append #$containerd "/bin/containerd"))
                ;; For finding containerd-shim binary.
                #:environment-variables
                (list (string-append "PATH=" #$containerd "/bin"))
                #:pid-file "/run/containerd/containerd.pid"
                #:pid-file-timeout 300
                #:log-file "/var/log/containerd.log"))
      (stop #~(make-kill-destructor))))))

(define containerd-service-type
  (service-type
   (name 'containerd)
   (extensions
    (list (service-extension shepherd-root-service-type
                             containerd-shepherd-service)))
   (default-value '())
   (description "Run containerd service.")))


;;;
;;; kubelet
;;;

(define-record-type* <kubelet-configuration>
  kubelet-configuration make-kubelet-configuration
  kubelet-configuration?
  (kubelet kubelet-configuration-kubelet ;string
           (default kubernetes))
  (log-file kubelet-configuration-log-file ;string
            (default "/var/log/kubelet.log"))
  (drbd? kubelet-configuration-drbd? ;boolean
         (default #f))
  (hpvolumes? kubelet-configuration-hpvolumes? ;boolean
              (default #f))
  (kubevirt? kubernetes-kubelet-configuration-kubevirt? ;boolean
             (default #f))
  (cilium?   kubernetes-kubelet-configuration-cilium?   ;boolean
             (default #f))
  (flux?     kubernetes-kubelet-configuration-flux?     ;boolean
             (default #f))
  (arguments kubelet-configuration-arguments ;list of strings
             (default '())))

(define (kubelet-log-rotations config)
  (list (log-rotation
         (files (list (kubelet-configuration-log-file config))))))

(define (kubelet-shepherd-service config)
  (list
   (shepherd-service
    (documentation "kubelet daemon.")
    (provision '(kubelet))
    (requirement '(networking containerd))
    (start #~(make-forkexec-constructor
              (list #$(program-file
                       "kubelet"
                       #~(begin
                           #$(maintenance)
                           #$(etcd)
                           #$(kubernetes-images)
                           #$(cilium-requirements)
                           '#$(if (kubelet-configuration-drbd? config)
                                  (drbd-requirements)
                                  '())
                           '#$(if (kubelet-configuration-hpvolumes? config)
                                  (hpvolumes-requirements)
                                  '())
                           #$(prlimit-requirements)
                           (execl #$(kubelet-configuration-kubelet config)
                                  "kubelet"
                                  #$@(kubelet-configuration-arguments config)))))
              #:environment-variables
              (list
               (string-append "PATH="
                              #$(file-append containerd "/bin")
                              ":" #$(file-append coreutils "/bin")
                              ":" #$(file-append grep "/bin")
                              ":" #$(file-append gzip "/bin")
                              ":" #$(file-append kmod "/bin")
                              ":" #$(file-append util-linux "/bin")
                              ":" #$(string-append
                                     (kubelet-configuration-kubelet config)
                                     "/bin")))
              #:log-file #$(kubelet-configuration-log-file config)))
    (stop #~(make-kill-destructor)))))

(define kubelet-service-type
  (service-type
   (name 'kubelet)
   (extensions
    (list (service-extension shepherd-root-service-type
                             kubelet-shepherd-service)
          (service-extension rottlog-service-type
                             kubelet-log-rotations)
          (service-extension profile-service-type
                             (lambda (config)
                               (append (list kubectl
                                             kubernetes-helm
                                             nerdctl
                                             k9s)
                                       (if (kubernetes-kubelet-configuration-cilium? config)
                                           (list cilium)
                                           '())
                                       (if (kubernetes-kubelet-configuration-flux? config)
                                           (list flux)
                                           '())
                                       (if (kubernetes-kubelet-configuration-kubevirt? config)
                                           (list virtctl)
                                           '()))))))
   (default-value (kubelet-configuration))
   (description "Run the kubelet.")))


;;;
;;;
;;;

(define-record-type* <edgecore-configuration>
  edgecore-configuration make-edgecore-configuration
  edgecore-configuration?
  (edgecore edgecore-configuration-edgecore ;string
            (default (file-append edgecore "/bin/edgecore")))
  (log-file edgecore-configuration-log-file ;string
            (default "/var/log/edgecore.log"))
  (arguments edgecore-configuration-arguments ;list of strings
             (default '())))

(define (edgecore-log-rotations config)
  (list (log-rotation
         (files (list (edgecore-configuration-log-file config))))))

(define (edgecore-wrapper args)
  (program-file
   "edgecore-wrapper"
   #~(begin
       #$(cilium-requirements)
       #$args)))

(define (edgecore-shepherd-service config)
  (list
   (shepherd-service
    (documentation "edgecore daemon.")
    (provision '(edgecore))
    (requirement '(networking containerd))
    (start #~(make-forkexec-constructor
              (list #$(edgecore-wrapper
                       #~(execl #$(edgecore-configuration-edgecore config)
                                "edgecore"
                                #$@(edgecore-configuration-arguments config))))
              #:log-file #$(edgecore-configuration-log-file config)))
    (stop #~(make-kill-destructor)))))

(define edgecore-service-type
  (service-type
   (name 'edgecore)
   (extensions
    (list (service-extension shepherd-root-service-type
                             edgecore-shepherd-service)
          (service-extension rottlog-service-type
                             edgecore-log-rotations)))
   (default-value '())
   (description "Run the edgecore.")))

;;; kubernetes.scm ends here
