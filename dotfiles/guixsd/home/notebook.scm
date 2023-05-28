(use-modules (gnu home)
             (gnu home services)
             (gnu home services mcron)
             (gnu home services shells)
             (gnu home services ssh)
	     (gnu services)
             (gnu packages xorg)
	     (guix gexp)
	     (home config)
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
             (home services video)
             (home services stumpwm))

(define xmodmap-script
  (program-file
   "xmodmap"
   #~(begin
       (use-modules (srfi srfi-1)
                    (ice-9 popen)
                    (ice-9 rdelim))

       (define %home
         (and=> (getenv "HOME")
                (lambda (home)
                  home)))

       (define xmodmap
         #$(file-append xmodmap "/bin/xmodmap"))

       (define count (make-parameter 0))

       (let loop ()
         (let* ((port (open-pipe* OPEN_READ xmodmap "-pke"))
                (output (read-string port)))
           (close-pipe port)
           (if (or (< 2 (count))
                   (any (lambda (str)
                          (string= str "keycode 134 = Control_L NoSymbol Control_L"))
                        (string-split (string-trim-right output #\newline) #\newline)))
               #t
               (begin
                 (count (1+ (count)))
                 (system* xmodmap (string-append %home "/.Xmodmap"))
                 (sleep 3)
                 (loop))))))))

(home-environment
 (services
  (list
   (simple-service 'sway-config
                   home-files-service-type
                   (list `(".config/sway/config" ,(local-file (string-append %project-directory "/dot_config/sway/notebook.config")))
                         `(".xkb/symbols/custom" ,(local-file (string-append %project-directory "/dot_xkb/symbols/custom")))
			 `(".config/sway/status.sh" ,(local-file (string-append %project-directory "/dot_config/sway/status.sh") #:recursive? #t))))
   (service stumpwm-service-type
            (let ((config-files
                   '("utils.lisp"
                     "keys.lisp"
                     "nav.lisp"
                     "theme.lisp"
                     "xorg.lisp"
                     "term.lisp"
                     "text-editors.lisp"
                     "repl.lisp"
                     "notify.lisp"
                     "hardware.lisp"
                     "admin.lisp"
                     "clipboard.lisp"
                     "screenshoot.lisp"
                     "password.lisp"
                     "trans.lisp"
                     "backup.lisp"
                     "documentation.lisp"
                     "emacs.lisp"
                     "chat.lisp"
                     "mail.lisp"
                     "docker.lisp"
                     "vnc.lisp"
                     "rofi.lisp"
                     "audio.lisp"
                     "mpv.lisp"
                     "streamlink.lisp"
                     "youtube-dl.lisp"
                     "android.lisp"
                     "kodi.lisp"
                     "web.lisp"
                     "time.lisp"
                     "mjru.lisp"
                     "virtualization.lisp"
                     "bittorrent.lisp"
                     "kubernetes.lisp"
                     "disk.lisp"
                     "rest.lisp"
                     "cpu.lisp"
                     "mem.lisp"
                     "imap.lisp"
                     "covid19.lisp"
                     "gpg.lisp"
                     "vpn.lisp"
                     "notebook/mode-line.lisp"
                     "display-0.lisp"
                     "display.lisp"
                     "autostart.lisp"
                     "swank.lisp"
                     "gaps.lisp"
                     )))
              (stumpwm-configuration
               (init-config
                `((in-package :stumpwm)

                  (run-shell-command "xrandr --output eDP --scale 0.5x0.5")
                  (run-shell-command "xmodmap /home/oleg/.xmodmap")

                  (require "asdf")

                  ;; https://discourse.nixos.org/t/fonts-in-nix-installed-packages-on-a-non-nixos-system/5871/9
                  (defvar *fontconfig-file*
                    "FONTCONFIG_FILE=/run/current-system/profile/etc/fonts/fonts.conf")

                  ;; (defcommand quassel () ()
                  ;;   (run-shell-command (join (list *fontconfig-file* "/home/oleg/.nix-profile/bin/quassel"))))

                  ;; Tuesday January 3 2005 23:05:25
                  (setq *time-format-string-default* "%A %B %e %Y %k:%M:%S")

                  (setf *startup-message* nil)
                  (setf *message-window-gravity* :center)
                  (setf *input-window-gravity* :center)
                  ,@(map (lambda (config-file)
                           `(load ,(string-append "/home/oleg/.stumpwm.d/" config-file)))
                         config-files)))
               (config-files config-files))))
   (simple-service 'xsession-config
                   home-activation-service-type
                   (let* ((stumpwp-load-file
                           (plain-file
                            "stumpwp-load-file"
                            (with-output-to-string
                              (lambda ()
                                (display '(require :asdf))
                                (newline)
                                (display '(require :stumpwm))
                                (newline)
                                (display '(stumpwm:stumpwm))
                                (newline)))))
                          (xsession-file
                           (program-file
                            "xsession"
                            #~(begin
                                (use-modules (srfi srfi-1)
                                             (ice-9 popen)
                                             (ice-9 rdelim)
                                             (ice-9 format))

                                (define %display
                                  (and=> (getenv "DISPLAY")
                                         (lambda (display)
                                           display)))

                                (define %home
                                  (and=> (getenv "HOME")
                                         (lambda (home)
                                           home)))

                                (display "Set background\n")
                                (system* #$(file-append xsetroot "/bin/xsetroot")
                                         "-solid" "black")

                                (display "Set cursor theme\n")
                                (system* #$(file-append xsetroot "/bin/xsetroot")
                                         "-cursor_name" "left_ptr")

                                (display "Disable speaker\n")
                                (system* #$(file-append xset "/bin/xset") "-b")

                                (display "Configure keymap\n")
                                (system* #$xmodmap-script)

                                (system* #$(file-append setxkbmap "/bin/setxkbmap")
                                         "-layout" "us,ru" "-option" "grp:win_space_toggle")

                                ;; Prepare environment for VNC sessions
                                (display "Start window manager\n")
                                (if (string= %display ":0.0")
                                    (execl "/run/current-system/profile/bin/sbcl" "sbcl" "--load" #$stumpwp-load-file)
                                    (begin
                                      (unsetenv "SESSION_MANAGER")
                                      (unsetenv "DBUS_SESSION_BUS_ADDRESS")
                                      (system* #$(file-append xhost "/bin/xhost") "+local:")
                                      (let* ((pw    (getpw (getuid)))
                                             (shell (passwd:shell pw)))
                                        ;; The '--login' option is supported at least by Bash and zsh.
                                        (execl shell "sbcl" "--login" "-c"
                                               (format #f ". /home/oleg/.bash_profile; /run/current-system/profile/bin/sbcl --load ~a"
                                                       #$stumpwp-load-file)))))))))
                     #~(begin
                         (let ((file #$(string-append %home "/.xsession")))
                           (copy-file #$xsession-file file)
                           (chmod file #o700)))))
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
   ;; home-shellcheck-service
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
