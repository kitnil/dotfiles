(use-modules (gnu packages admin)
             (gnu packages android)
             (gnu packages aspell)
             (gnu packages backup)
             (gnu packages bittorrent)
             (gnu packages code)
             (gnu packages commencement)
             (gnu packages databases)
             (gnu packages dictionaries)
             (gnu packages emacs)
             (gnu packages file)
             (gnu packages fonts)
             (gnu packages fontutils)
             (gnu packages gcc)
             (gnu packages gnome)
             (gnu packages gnupg)
             (gnu packages gnuzilla)
             (gnu packages graphviz)
             (gnu packages guile)
             (gnu packages image-viewers)
             (gnu packages kodi)
             (gnu packages libreoffice)
             (gnu packages linux)
             (gnu packages lisp)
             (gnu packages mail)
             (gnu packages package-management)
             (gnu packages password-utils)
             (gnu packages pulseaudio)
             (gnu packages readline)
             (gnu packages screen)
             (gnu packages ssh)
             (gnu packages version-control)
             (gnu packages video)
             (gnu packages virtualization)
             (gnu packages web)
             (gnu packages wget)
             (gnu packages xdisorg)
             (gnu packages xorg)
             (wigust packages emacs))

(define %desktop-packages
  ;; Basic desktop tasks.
  (list adwaita-icon-theme ; Applications icons (for Icecat and others)

        ;; Spelling dictionaries
        aspell
        aspell-dict-en  ; English
        aspell-dict-ru  ; Russian

        libreoffice     ; Office suite

        graphviz        ; Graphics dot

        translate-shell ; Translation in CLI and Emacs

        git             ; Version control
        magit           ; Emacs interface for Git

        icecat          ; Web browser

        isync           ; Sync IMAP
        notmuch         ; Mail indexer based on Xapian

        feh             ; Image viewer
        mpv             ; Video and audio player
        ffmpeg          ; Video, audio, images, gif conversion

        gnu-c-manual    ; C language documentation

        qemu            ; Virtualization

        adb             ; For Replicant (Android distribution) control

        rdiff-backup    ; Incremental backups

        ;; Encryption and signing
        gnupg
        pinentry        ; Password typing for Gnupg

        password-store  ; Password management

        recutils        ; Filter records like in `guix --search'

        font-dejavu     ; General fonts
        font-liberation ; Times New Roman replcement
        font-awesome    ; Icons in fonts
        font-wqy-zenhei ; Asia fonts
        fontconfig      ; Make `fc-cache -f' after installing addional fonts.

        pavucontrol     ; Pulseaudio control GUI
        pulsemixer      ; Pulseaudio control CLI

        sbcl   ; For StumpWM.  See <https://stumpwm.github.io/>.

        ;; For helm-stumpwm-commands and stumpish
        rlwrap
        xprop

        ;; $EDITOR
        emacs                    ; The best editor
        emacs-aggressive-indent  ; Auto indent minor mode
        emacs-company            ; Complition framework
        emacs-company-quickhelp  ; Help pages for Company
        emacs-debbugs            ; <https://debbugs.gnu.org/> interface
        emacs-debpaste           ; Front end to <https://paste.debian.net/>
        emacs-elfeed             ; RSS reader
        emacs-engine-mode        ; Define searches on websites
        emacs-erc-hl-nicks       ; for ERC
        emacs-eval-in-repl       ; Evaluate to different Repls
        emacs-ewmctrl            ; Control X windows from Emacs
        emacs-ggtags             ; Front end to GNU Global
        emacs-gitpatch           ; Send patches
        emacs-guix               ; Guix interface
        emacs-helm               ; Narrowing framework
        emacs-helm-firefox       ; Search for bookmarks in Icecat
        emacs-helm-make          ; Front end to `make'
        emacs-helm-pass          ; Front end to password-store
        emacs-helm-projectile    ; Helm interface for Projectile
        emacs-highlight-stages   ; Highlight code stages
        emacs-markdown-mode      ; Commonmark major mode
        emacs-multiple-cursors   ; Multi cursor
        emacs-nix-mode           ; Nix language mode
        emacs-org-mind-map       ; General mind maps from Org files
        emacs-projectile         ; Project functions
        emacs-slime              ; Sbcl repl
        emacs-smartparens        ; Structured editing
        emacs-strace-mode        ; Colorize `strace' logs
        emacs-transmission       ; Front end to transmission-daemon
        emacs-transpose-frame    ; M-x transpose-frame
        emacs-use-package        ; Lazy configuration
        emacs-w3m                ; Front end to w3m command line web browser
        emacs-which-key          ; Key bindings help
        emacs-yasnippet          ; Snippets
        emacs-yasnippet-snippets ; Collection of snippets
        flycheck                 ; Syntax checker
        geiser                   ; Scheme bridge

        gcc-toolchain ; For Emacs `semantic-mode'
        cflow         ; C program call map
        global        ; Source tagging

        haunt            ; Guile static site generator
        guile-commonmark ; Commonmark for Guile

        gwl              ; Guix workflow management

        file   ; Show type of a file
        htop   ; Pretty top
        netcat ; TCP calls

        ;; X11 utils.  See <~/.xsession> and <~/.stumpwm.d/init.lisp>.
        setxkbmap   ; Keyboard layout
        xclip       ; Access clipboard from CLI
        xrdb
        xset
        xsetroot
        xterm       ; $TERM
        xorg-server ; For testing Stumpwm.
        wmctrl      ; For emacs-ewmctrl
        xwininfo    ; Information about X11 window

        lm-sensors ; Watch for temp of hardware

        openssh    ; SSH

        screen     ; Terminal multiplexer

        redshift   ; Eyes saver.

        ;; Downloaders.
        transmission ; Bittorrent
        kodi-cli     ; Remote control Kodi
        youtube-dl   ; Video and music from websites
        wget))

(packages->manifest %desktop-packages)
