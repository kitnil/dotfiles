(define-module (wugi system vm-guixsd)
  #:use-module (gnu)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages android)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages firmware)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages nfs)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services avahi)
  #:use-module (gnu services certbot)
  #:use-module (gnu services cgit)
  #:use-module (gnu services databases)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services dns)
  #:use-module (gnu services docker)
  #:use-module (gnu services linux)
  #:use-module (gnu services mail)
  #:use-module (gnu services mcron)
  #:use-module (gnu services monitoring)
  #:use-module (gnu services networking)
  #:use-module (gnu services nfs)
  #:use-module (gnu services nix)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services sound)
  #:use-module (gnu services ssh)
  #:use-module (gnu services sysctl)
  #:use-module (gnu services version-control)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services vpn)
  #:use-module (gnu services web)
  #:use-module (gnu services xorg)
  #:use-module (gnu system setuid)
  #:use-module (guix channels)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix inferior)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (wugi config)
  #:use-module (wugi manifests deprecated)
  #:use-module (wugi packages certs)
  #:use-module (wugi system hardware vm-guixsd)
  #:use-module (wugi packages linux)
  #:use-module (wugi packages linux-modules)
  #:use-module (wugi packages monitoring)
  #:use-module (wugi packages netboot)
  #:use-module (wugi services admin)
  #:use-module (wugi services autofs)
  #:use-module (wugi services autossh)
  #:use-module (wugi services backup)
  #:use-module (wugi services bird)
  #:use-module (wugi services bittorrent)
  #:use-module (wugi services certbot)
  #:use-module (wugi services desktop)
  #:use-module (wugi services docker)
  #:use-module (wugi services ipset)
  #:use-module (wugi services jenkins)
  #:use-module (wugi services kubernetes)
  #:use-module (wugi services monitoring)
  #:use-module (wugi services networking)
  #:use-module (wugi services nix)
  #:use-module (wugi services openvpn)
  #:use-module (wugi services syncthing)
  #:use-module (wugi services virtualization)
  #:use-module (wugi services web)
  #:use-module (wugi utils)
  #:use-module (wugi utils package)
  #:export (%vm-guixsd))

(define %vm-guixsd-private-ip-address
  "192.168.0.145")

(define (%vm-guixsd)
  (let ((base-system (%vm-guixsd-hardware)))
    (operating-system
      (inherit base-system)

      (keyboard-layout (keyboard-layout "us" "altgr-intl"))

      (kernel-loadable-modules (list drbd-module))

      (packages %my-system-packages)

      (groups (append (list (user-group (name "uinput")))
                      %base-groups))

      (users
       (append
        (list (user-account
               (name "oleg")
               (uid 1000)
               (comment "Oleg Pykhalov")
               (group "users")
               (supplementary-groups
                '("wheel" "audio" "video" "kvm" "input"))
               (home-directory "/home/oleg")))
        %base-user-accounts))

      (hosts-file
       (generate-hosts-file
        '("127.0.0.1 guixsd localhost home.wugi.info gitlab.wugi.info"
          "::1 guixsd localhost")))

      (services
       (append
        (list
         ;; Raise the maximum number of open file descriptors
         ;; that can be used.
         (service pam-limits-service-type
                  (list
                   (pam-limits-entry "*" 'both 'nofile 100000)))

         (extra-special-file "/usr/bin/env"
                             (file-append coreutils "/bin/env"))

         ;; (service knot-resolver-service-type
         ;;          (knot-resolver-configuration
         ;;           (kresd-config-file
         ;;            (generate-kresd-file %vm-guixsd-private-ip-address))))

         (service openssh-service-type
                  (openssh-configuration
                   (authorized-keys
                    `(("jenkins" ,(local-file
                                   (string-append
                                    %distro-directory
                                    "/dotfiles/guixsd/ssh/id_rsa_jenkins.wugi.info.pub")))))
                   (x11-forwarding? #t)
                   (gateway-ports? 'client)
                   (permit-root-login 'prohibit-password)
                   (password-authentication? #f)
                   (use-pam? #f)
                   (extra-content "\
Match Address 127.0.0.1
PasswordAuthentication yes")))

         (service containerd-service-type)

         ;; (service kubelet-service-type
         ;;          (kubelet-configuration
         ;;           (kubelet "/nix/store/lp8ch8l5dn4bcp056cpr1gfyb9i8zi54-kubernetes-1.25.4/bin/kubelet")
         ;;           (arguments
         ;;            '("--address=192.168.0.144"
         ;;              "--node-ip=192.168.0.144"
         ;;              "--authentication-token-webhook"
         ;;              "--authentication-token-webhook-cache-ttl=10s"
         ;;              "--authorization-mode=Webhook"
         ;;              "--client-ca-file=/etc/kubernetes/pki/ca.pem"
         ;;              "--cluster-dns=10.8.255.254"
         ;;              "--cluster-domain=cluster.local"
         ;;              "--hairpin-mode=hairpin-veth"
         ;;              "--healthz-bind-address=127.0.0.1"
         ;;              "--healthz-port=10248"
         ;;              "--hostname-override=kube1"
         ;;              "--kubeconfig=/home/oleg/src/cgit.wugi.info/wigust/dotfiles/guix/dotfiles/kubernetes/kubeconfig"
         ;;              "--pod-infra-container-image=pause"
         ;;              "--port=10250"
         ;;              "--register-node=true"
         ;;              "--register-with-taints=unschedulable=true:NoSchedule"
         ;;              "--root-dir=/var/lib/kubelet"
         ;;              "--tls-cert-file=/etc/kubernetes/pki/kubelet-client-kube1.pem"
         ;;              "--tls-private-key-file=/etc/kubernetes/pki/kubelet-client-kube1-key.pem"
         ;;              "--container-runtime=remote"
         ;;              "--container-runtime-endpoint=unix:///run/containerd/containerd.sock"
         ;;              "--fail-swap-on=false"
         ;;              "--eviction-hard=nodefs.available<10Gi,nodefs.inodesFree<1000000,imagefs.available<10Gi,imagefs.inodesFree<1000000"
         ;;              "--image-gc-high-threshold=95"
         ;;              "--image-gc-low-threshold=90"
         ;;              "--pod-manifest-path=/etc/kubernetes/manifests"
         ;;              "--max-pods=200"))
         ;;           (drbd? #t)
         ;;           (hpvolumes? #t)
         ;;           (cilium? #t)
         ;;           (flux? #t)
         ;;           (kubevirt? #t)))

         (service kernel-module-loader-service-type
                  '("dm-snapshot"
                    "dm-thin-pool"
                    "br_netfilter" ;kube-dns

                    ;; Required for Cilium CNI.
                    "ip_tables"
                    "xt_socket"
                    "iptable_nat"
                    "iptable_mangle"
                    "iptable_raw"
                    "iptable_filter"))

         ;; Bring eth0 up and pass it to the networking bridge.
         (service static-networking-service-type
                  (list
                   (static-networking
                    (provision '(eth0))
                    (addresses (list
                                (network-address
                                 (device "eth0")
                                 (value "127.0.0.2/8")))))
                   (static-networking
                    (provision '(br0-link))
                    (links (list
                            (network-link
                             (name "br0")
                             (type 'bridge)
                             (arguments '()))))
                    (addresses '()))
                   (static-networking
                    (provision '(br0))
                    (requirement '(br0-link))
                    (addresses (list
                                (network-address
                                 (device "br0")
                                 (value "192.168.0.145/24"))))
                    (routes
                     (list (network-route
                            (destination "default")
                            (gateway "192.168.0.1"))))
                    (name-servers '("192.168.0.144")))
                   (static-networking
                    (provision '(networking))
                    (requirement '(eth0 br0))
                    (links (list
                            (network-link
                             (name "eth0")
                             (arguments '((master . "br0"))))))
                    (addresses '()))))

         (service ipset-service-type)
         (udisks-service)
         (service accountsservice-service-type)
         (service colord-service-type)
         (geoclue-service)
         (service polkit-service-type)
         (dbus-service #:services (list avahi))
         (elogind-service)
         (service ntp-service-type))

        (modify-services (operating-system-user-services base-system)
          (guix-service-type config => (guix-configuration
                                        (inherit %guix-daemon-config)
                                        (substitute-urls '("https://bordeaux.guix.gnu.org"
                                                           "https://substitutes.nonguix.org"))
                                        (extra-options '("--cache-failures"))))
          (sysctl-service-type _ =>
                               (sysctl-configuration
                                (settings (append '(("net.bridge.bridge-nf-call-iptables" . "0")
                                                    ;; Allow to forward ingress traffic from public to 127.0.0.0/8 via DNAT.
                                                    ("net.ipv4.conf.all.route_localnet" . "1")
                                                    ;; Allow to bind services to sockets while address on a network interface is not available.
                                                    ("net.ipv4.ip_nonlocal_bind" . "1")
                                                    ;; opensearch requirement
                                                    ("vm.max_map_count" . "262144")
                                                    ;; piraeus piraeus-op-ns-node requirement
                                                    ("fs.inotify.max_user_watches" . "100000")
                                                    ("fs.inotify.max_user_instances" . "100000"))
                                                  %default-sysctl-settings)))))))

      (setuid-programs %my-setuid-programs)

      (sudoers-file (plain-file "sudoers"
                                (string-join `("Defaults:root runcwd=*"
                                               "root ALL=(ALL) ALL"
                                               "%wheel ALL=(ALL) ALL"
                                               "oleg ALL=(ALL) NOPASSWD:ALL")
                                             "\n")))

      ;; Allow resolution of '.local' host names with mDNS.
      (name-service-switch %mdns-host-lookup-nss))))
