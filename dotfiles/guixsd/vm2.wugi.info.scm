;; This is an operating system configuration template
;; for a "bare bones" setup, with no X11 display server.

(use-modules (gnu))
(use-service-modules desktop dbus docker networking linux nix monitoring ssh)
(use-package-modules certs linux screen ssh)

(use-modules (config)
             (services dns)
             (services ipset)
             (services networking)
             (services kubernetes))

(operating-system
  (host-name "vm2.wugi.info")
  (timezone "Europe/Moscow")
  (locale "en_US.utf8")

  ;; Boot in "legacy" BIOS mode, assuming /dev/sdX is the
  ;; target hard disk, and "my-root" is the label of the target
  ;; root file system.
  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (target "/dev/vda")))
  (file-systems (cons (file-system
                        (device (file-system-label "guix-root"))
                        (mount-point "/")
                        (type "ext4"))
                      %base-file-systems))

  ;; This is where user accounts are specified.  The "root"
  ;; account is implicit, and is initially created with the
  ;; empty password.
  (users (cons (user-account
                (name "oleg")
                (comment "Oleg Pykhalov")
                (group "users")

                ;; Adding the account to the "wheel" group
                ;; makes it a sudoer.  Adding it to "audio"
                ;; and "video" allows the user to play sound
                ;; and access the webcam.
                (supplementary-groups '("wheel"
                                        "audio" "video")))
               %base-user-accounts))

  (hosts-file
   (plain-file
    "hosts"
    (string-join
     (list "127.0.0.1 vm2.wugi.info localhost"
           "93.100.15.190 ci.guix.gnu.org.wugi.info"
           "192.168.26.1 kube10001"
           "::1 vm2.wugi.info localhost"
           "\n")
     "\n")))

  ;; Globally-installed packages.
  (packages (append (list ipset iptables screen)
                    %base-packages))

  ;; Add services to the baseline.
  (services (append (list (service static-networking-service-type
                                  (list (static-networking
                                         (addresses
                                          (list (network-address
                                                 (device "eth0")
                                                 (value "78.108.92.69/23"))
                                                (network-address
                                                 (device "eth0")
                                                 (value "192.168.26.1/24"))))
                                         (routes
                                          (list (network-route
                                                 (destination "default")
                                                 (gateway "78.108.93.254"))))
                                         (name-servers '("127.0.0.1"
                                                         "8.8.8.8"
                                                         "8.8.4.4")))))
                          (service crowdsec-service-type)
                          (service crowdsec-firewall-bouncer-service-type)
                          (service containerd-service-type)
                          (dbus-service)
                          (elogind-service)
                          (service docker-service-type)
                          (service kubelet-service-type
                                   (kubelet-configuration
                                    (kubelet "/nix/store/lp8ch8l5dn4bcp056cpr1gfyb9i8zi54-kubernetes-1.25.4/bin/kubelet")
                                    (cilium? #t)
                                    (flux? #t)
                                    (arguments
                                     '("--address=192.168.26.1"
                                       "--node-ip=192.168.26.1"
                                       "--authentication-token-webhook"
                                       "--authentication-token-webhook-cache-ttl=10s"
                                       "--authorization-mode=Webhook"
                                       "--client-ca-file=/etc/kubernetes/pki/ca.pem"
                                       "--cluster-dns=10.16.255.254"
                                       "--cluster-domain=cluster.local"
                                       "--hairpin-mode=hairpin-veth"
                                       "--healthz-bind-address=127.0.0.1"
                                       "--healthz-port=10248"
                                       "--hostname-override=kube10001"
                                       "--kubeconfig=/etc/kubernetes/cluster-admin.kubeconfig"
                                       "--pod-infra-container-image=pause"
                                       "--port=10250"
                                       "--register-node=true"
                                       "--register-with-taints=unschedulable=true:NoSchedule"
                                       "--root-dir=/var/lib/kubelet"
                                       "--tls-cert-file=/etc/kubernetes/pki/kubelet-client-kube10001.pem"
                                       "--tls-private-key-file=/etc/kubernetes/pki/kubelet-client-kube10001-key.pem"
                                       "--container-runtime=remote"
                                       "--container-runtime-endpoint=unix:///run/containerd/containerd.sock"
                                       "--fail-swap-on=false"
                                       "--eviction-hard=nodefs.available<5Gi,nodefs.inodesFree<500000,imagefs.available<5Gi,imagefs.inodesFree<500000"
                                       "--image-gc-high-threshold=95"
                                       "--image-gc-low-threshold=90"
                                       "--pod-manifest-path=/etc/kubernetes/manifests"))))
                          (service ntp-service-type
                                   (ntp-configuration
                                    (servers
                                     (list
                                      (ntp-server
                                       (type 'pool)
                                       (address "78.108.93.254")
                                       (options '("iburst")))))))
                          (service knot-dns-service-type
                                   (knot-dns-configuration
                                    (config-file
                                     (knot-config "78.108.92.69"))))
                          (service openssh-service-type)
                          (service zram-device-service-type
                                   (zram-device-configuration
                                    (size "8G")))
                          (service prometheus-node-exporter-service-type)
                          (service nix-service-type
                                   (nix-configuration
                                    (extra-config '("trusted-users = oleg root"))))
                          (service ipset-service-type
                                   (ipset-configuration
                                    (iptables? #t)))
                          ;; (service iptables-service-type
                          ;;          (iptables-configuration
                          ;;           (ipv4-rules (local-file "etc/iptables/iptables.rules"))
                          ;;           (ipv6-rules (local-file "etc/iptables/ip6tables.rules"))))
                          )
                    (modify-services %base-services
                      (guix-service-type config => %guix-daemon-config-with-substitute-urls))))

  (sudoers-file (plain-file "sudoers" "\
Defaults:root runcwd=*
root ALL=(ALL) ALL
%wheel ALL=(ALL) ALL
oleg ALL=(ALL) NOPASSWD:ALL\n")))
