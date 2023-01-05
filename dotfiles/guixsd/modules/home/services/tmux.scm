(define-module (home services tmux)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (home config)
  #:export (home-tmux-service
            tmuxifier-service))

(define home-tmux-service
  (simple-service 'tmux-config
                  home-files-service-type
                  (list `(".tmux.conf"
                          ,(local-file (string-append %project-directory "/dot_tmux.conf"))))))

(define tmuxifier-service
  (simple-service 'tmuxifier-config
                  home-files-service-type
                  (list `(".tmuxifier-layouts/backup.session.sh"
                          ,(local-file (string-append %project-directory "/dot_tmuxifier-layouts/backup.session.sh")))
                        `(".tmuxifier-layouts/backup.window.sh"
                          ,(local-file (string-append %project-directory "/dot_tmuxifier-layouts/backup.window.sh")))
                        `(".tmuxifier-layouts/blog.session.sh"
                          ,(local-file (string-append %project-directory "/dot_tmuxifier-layouts/blog.session.sh")))
                        `(".tmuxifier-layouts/blog.window.sh"
                          ,(local-file (string-append %project-directory "/dot_tmuxifier-layouts/blog.window.sh")))
                        `(".tmuxifier-layouts/guix-machines.window.sh"
                          ,(local-file (string-append %project-directory "/dot_tmuxifier-layouts/guix-machines.window.sh")))
                        `(".tmuxifier-layouts/guix.session.sh"
                          ,(local-file (string-append %project-directory "/dot_tmuxifier-layouts/guix.session.sh")))
                        `(".tmuxifier-layouts/guix.window.sh"
                          ,(local-file (string-append %project-directory "/dot_tmuxifier-layouts/guix.window.sh")))
                        `(".tmuxifier-layouts/elk.session.sh"
                          ,(local-file (string-append %project-directory "/dot_tmuxifier-layouts/elk.session.sh")))
                        ;; TODO: `("web.session.sh.tmpl" ,(local-file "../../dot_tmuxifier-layouts/web.session.sh.tmpl"))
                        `(".tmuxifier-layouts/elk.window.sh"
                          ,(local-file (string-append %project-directory "/dot_tmuxifier-layouts/elk.window.sh")))
                        `(".tmuxifier-layouts/kubernetes-cert-manager-logs.window.sh"
                          ,(local-file (string-append %project-directory "/dot_tmuxifier-layouts/kubernetes-cert-manager-logs.window.sh")))
                        `(".tmuxifier-layouts/kubernetes-cilium.window.sh"
                          ,(local-file (string-append %project-directory "/dot_tmuxifier-layouts/kubernetes-cilium.window.sh")))
                        `(".tmuxifier-layouts/kubernetes-flux-logs.window.sh"
                          ,(local-file (string-append %project-directory "/dot_tmuxifier-layouts/kubernetes-flux-logs.window.sh")))
                        `(".tmuxifier-layouts/kubernetes-flux.window.sh"
                          ,(local-file (string-append %project-directory "/dot_tmuxifier-layouts/kubernetes-flux.window.sh")))
                        `(".tmuxifier-layouts/kubernetes-kube-system.window.sh"
                          ,(local-file (string-append %project-directory "/dot_tmuxifier-layouts/kubernetes-kube-system.window.sh")))
                        `(".tmuxifier-layouts/kubernetes-piraeus.window.sh"
                          ,(local-file (string-append %project-directory "/dot_tmuxifier-layouts/kubernetes-piraeus.window.sh")))
                        `(".tmuxifier-layouts/kubernetes.session.sh"
                          ,(local-file (string-append %project-directory "/dot_tmuxifier-layouts/kubernetes.session.sh"))))))
