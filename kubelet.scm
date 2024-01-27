(use-modules (gnu packages base)
             (gnu packages bash)
             (gnu packages compression)
             (gnu packages docker)
             (gnu packages linux)
             (gnu packages package-management)
             (guix build utils)
             (guix gexp)
             (guix store)
             (packages kubernetes))

(define coredns-image
  (local-file "/nix/store/1crdy15nv25jpbvknrzyhg6khv9ikhl9-docker-image-coredns-coredns-1.7.1.tar"))

(define pause-image
  (local-file "/nix/store/xjlwhyqjhx0j2sc41wfpsw1zvhn98vh5-docker-image-pause.tar.gz"))

(run-with-store (open-connection)
  (gexp->derivation
   "kubelet"
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (guix build utils))
         (let* ((bin (string-append #$output "/bin"))
                (kubelet.sh (string-append bin "/kubelet.sh")))
           (mkdir-p bin)
           (copy-file #$(local-file "dot_local/bin/executable_kubelet")
                      kubelet.sh)
           (chmod kubelet.sh #o555)
           (setenv "PATH" (string-append #$(file-append tar "/bin")
                                         ":" #$(file-append gzip "/bin")
                                         ":" #$(file-append bash "/bin")))
           (patch-shebang kubelet.sh)
           (wrap-program kubelet.sh
             '("KUBELET_COREDNS_IMAGE" "" = (#$coredns-image))
             '("KUBELET_PAUSE_IMAGE" "" = (#$pause-image))
             '("PATH" ":" = (#$(file-append containerd "/bin")
                             #$(file-append coreutils "/bin")
                             #$(file-append grep "/bin")
                             #$(file-append gzip "/bin")
                             #$(file-append kmod "/bin")
                             "/nix/store/lp8ch8l5dn4bcp056cpr1gfyb9i8zi54-kubernetes-1.25.4/bin"
                             #$(file-append util-linux "/bin")))))))))

