;;;
;;; Default package system
;;;

(setq load-prefer-newer t)
(package-initialize)


;;;
;;; User
;;;

(setq user-mail-address "go.wigust@gmail.com")
(setq user-full-name "Oleg Pykhalov")


;;;
;;; Disable GUI
;;;

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)


;;;
;;; Theme
;;;

(custom-set-faces '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd"
                                         :slant normal :weight normal
                                         :height 143 :width normal)))))


;;;
;;; ERC
;;;

(use-package erc
  :bind (("C-c e l" . erc-connect-localhost)
         ("C-c e a" . erc-connect-all)
         ("C-c e f" . erc-connect-freenode)
         ("C-c e d" . erc-connect-debian)
         ("C-c e g" . erc-connect-gitter)
         ("C-c e G" . erc-connect-gnome)
         ("C-c e t" . erc-connect-twitch))
  :init
  (progn
    (setq erc-accidental-paste-threshold-seconds 0.5)
    (setq erc-autojoin-mode t)
    (setq erc-autojoin-timing (quote ident))
    (setq erc-email-userid "go.wigust@gmail.com")
    (setq erc-flood-protect t)
    (setq erc-hide-timestamps t)
    (setq erc-join-buffer (quote bury))
    (setq erc-kill-buffer-on-part nil)
    (setq erc-kill-server-buffer-on-quit nil)
    (setq erc-log-insert-log-on-open t)
    (setq erc-modules (quote (autojoin button completion fill irccontrols list
                                       log match menu move-to-prompt netsplit
                                       networks noncommands readonly ring
                                       smiley stamp track)))
    (setq erc-networks-alist nil)
    (setq erc-nick-uniquifier nil)
    (setq erc-server-auto-reconnect t)
    (setq erc-server-reconnect-attempts t)
    (setq erc-server-reconnect-timeout 60)
    (setq erc-track-position-in-mode-line (quote t))
    (setq erc-track-priority-faces-only (quote all))
    (setq erc-track-switch-direction (quote importantce))
    (setq erc-try-new-nick-p nil)
    (setq erc-user-full-name "Oleg Pykhalov")
    (setq erc-whowas-on-nosuchnick t)
    (setq erc-track-exclude-types '("NICK" "333" "353" "JOIN" "QUIT" "PART"))

    (defun erc-connect-localhost ()
      "Connect to localhost irc network"
      (interactive)
      (erc :server "localhost"
           :port 6667
           :nick "natsu"
           :password nil))

    (defun erc-connect-twitch ()
      "Connect to twitch irc network"
      (interactive)
      (add-to-list 'erc-networks-alist '(twitch "irc.chat.twitch.tv"))
      (erc-tls :server "irc.chat.twitch.tv"
               :port 6697
               :nick "wigust"
               :password nil))

    (defun erc-connect-globalgamers ()
      "Connect to globalgamers irc network"
      (interactive)
      (add-to-list 'erc-networks-alist '(globalgamers "irc.globalgamers.net"))
      (erc-tls :server "irc.globalgamers.net"
               :port 6660
               :nick "wigust"
               :password nil))

    (defun erc-connect-indymedia ()
      "Connect to indymedia irc network"
      (interactive)
      (add-to-list 'erc-networks-alist '(indymedia "irc.indymedia.org"))
      (erc-tls :server "irc.indymedia.org"
               :port 6697
               :nick "wigust"
               :password nil))

    (defun erc-connect-gitter ()
      "Connect to gitter irc network"
      (interactive)
      (add-to-list 'erc-networks-alist '(gitter "irc.gitter.im"))
      (erc-tls :server "irc.gitter.im"
               :port 6697
               :nick "wigust"
               :password nil))

    (defun erc-connect-gnome ()
      "Connect to gnome irc network"
      (interactive)
      (erc-tls :server "irc.gnome.org"
               :port 6697
               :nick "wigust"))

    (defun erc-connect-freenode ()
      "Connect to freenode irc network"
      (interactive)
      (erc-tls :server "irc.freenode.net"
               :port 6697
               :nick "wigust"
               :password nil))

    (defun erc-connect-debian ()
      "Connect to debian irc network"
      (interactive)
      (erc-tls :server "irc.oftc.net"
               :port 6697
               :nick "wigust"))

    (defun erc-connect-rizon ()
      "Connect to highway irc network"
      (interactive)
      (erc-tls :server "irc.rizon.net"
               :port 6697
               :nick "wigust"))

    (defun erc-connect-highway ()
      "Connect to highway irc network"
      (interactive)
      (erc-tls :server "irc.irchighway.net"
               :port 6697
               :nick "wigust"))

    (defun erc-connect-all ()
      "Connect to all configured irc networks"
      (interactive)
      (erc-connect-localhost) (erc-connect-debian)
      (erc-connect-freenode) (erc-connect-gnome)
      (erc-connect-gitter) (erc-connect-twitch)
      (erc-connect-rizon) (erc-connect-globalgamers)
      ;; (erc-connect-highway) ; No autojoin channels
      (erc-connect-indymedia))

    (defvar irc-gnome-servers '("umu.se" "gimp.net" "gimp.ca"
                                "gnome.org" "y.se" "poop.nl"))

    (defvar irc-gnome-channels '("#bugs" "#docs" "#gnome" "#gnome-hackers"
                                 "#gnome-shell" "#newcomers"))

    (defun irc-netlist (irc-networks irc-channels)
      (let (irc-netlist)
        (dolist (irc-network irc-networks irc-netlist)
          (if (equal irc-netlist nil)
              (setq irc-netlist
                    (list (cons irc-network irc-channels)))
            (setq irc-netlist (append
                               irc-netlist
                               (list (cons irc-network irc-channels))))))))

    (defvar irc-netlist-gnome (irc-netlist irc-gnome-servers
                                           irc-gnome-channels))

    (setq erc-autojoin-channels-alist
          (quote
           (("freenode.net" "#icecat" "#emacs" "##math"
             "##c"
             ;; "#clojure"
             ;; "#fedora" ;; "#fedora-admin" ;; "#fedora-devel"
             ;; "#fedora-noc" ;; "#fedora-meeting" ;; "#fedora-qa"
             "#gnu" "#fsf" "#gnus" "#guile" "#guix" "#stumpwm"
             ;; "#nixos" ;; "#grub" ;; "#haskell" ;; "#xmonad"
             ;; "#filmsbykris" ;; "##japanese" ;; "#latex"
             ;; "#python" ;; "#scipy" ;; "#sagemath"
             "#lisp" "#scheme")
            ("indymedia.org" "#riseup")
            ("gitter.im")
            ("oftc.net" "#debian" "#debian-next")
            ("globalgamers" "#Touhou")
            ("twitch.tv" "#tsoding" "#cattzs" "#retched"
             "#bbsssssssss" "#team_treehouse" "#rw_grim")
            ("uworld.se" "#coalgirls"))))

    (defun erc-netlist (irc-netlist)
      (dolist (irc-net irc-netlist)
        (append erc-autojoin-channels-alist irc-net)))

    (setq erc-autojoin-channels-alist
          (append erc-autojoin-channels-alist irc-netlist-gnome)))
  :config
  (progn
    (eval-after-load 'erc
      '(progn
         (erc-track-mode t)
         (erc-log-mode)
         (require 'erc-fill)
         (erc-fill-mode t)))

    (add-hook 'erc-mode-hook
              '(lambda ()
                 (require 'erc-pcomplete)
                 (pcomplete-erc-setup)
                 (erc-completion-mode 1)
                 (erc-ring-mode 1)
                 (setq pcomplete-ignore-case t)))
    (add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
    ;; bug#18527: 24.3; ERC does not reconnect when server disconnects me
    ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2015-12/msg01414.html
    (add-hook 'erc-disconnected-hook
              #'(lambda (nick host-name reason)
                  ;; Re-establish the connection even if the server closed it.
                  (setq erc-server-error-occurred nil)))))

(use-package erc-hl-nicks
  :after erc)
