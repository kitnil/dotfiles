(use-modules (gnu home)
             (gnu home services)
             (gnu home services mcron)
             (gnu home services shells)
             (gnu home services ssh)
	     (gnu services)
	     (guix gexp)
	     (home config)
	     (home services admin)
	     (home services shell)
             (home services mime)
             (home services gdb)
             (home services version-control)
             (home services emacs)
             (home services tmux)
             (home services linux)
             (home services nix)
             (home services terminals)
             (home services haskell-apps)
             (home services gtk)
             (home services rust-apps)
             (home services lisp)
             (home services python)
             (home services nano)
             (home services dns)
             (home services web)
             (home services gnupg)
             (home services groovy)
             (home services guile)
             (home services kodi)
             (home services mail)
             (home services databases)
             (home services video))

(home-environment
 (services
  (list
   (simple-service 'sway-config
                   home-files-service-type
                   (list `(".config/sway/config" ,(local-file (string-append %project-directory "/dot_config/sway/notebook.config")))
			 `(".config/sway/status.sh" ,(local-file (string-append %project-directory "/dot_config/sway/status.sh") #:recursive? #t))))
   home-bash-service
   home-mime-service
   home-direnv-service
   home-git-service
   home-gita-service
   home-gdb-service
   home-emacs-service
   home-nano-service
   home-inputrc-service
   home-tmux-service
   home-top-service
   home-htop-service
   home-nix-service
   home-alacritty-service
   home-qterminal-service
   home-greenclip-service
   home-gtk-service
   home-gtkrc-service
   home-ripgrep-service
   home-screen-service
   home-sbcl-service
   home-python-service
   home-bind-utils-service
   home-shellcheck-service
   home-bin-service
   home-chromium-service
   home-gnupg-service
   home-ghci-service
   home-groovy-service
   home-guile-service
   home-kodi-service
   home-mailcap-service
   home-mongo-service
   home-postgresql-service
   home-mycli-service
   home-parallel-service
   home-youtube-dl-service
   home-mpv-service)))
