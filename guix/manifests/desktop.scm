(use-modules (gnu packages aspell)
             (gnu packages backup)
             (gnu packages databases)
             (gnu packages emacs)
             (gnu packages fonts)
             (gnu packages fontutils)
             (gnu packages gnome)
             (gnu packages gnupg)
             (gnu packages gnuzilla)
             (gnu packages image-viewers)
             (gnu packages lisp)
             (gnu packages mail)
             (gnu packages password-utils)
             (gnu packages screen)
             (gnu packages version-control)
             (gnu packages video)
             (gnu packages virtualization)
             (gnu packages web)
             (gnu packages wget)
             (gnu packages xdisorg)
             (gnu packages xorg))

(define %desktop-packages
  ;; Basic desktop tasks.
  (list adwaita-icon-theme ; Applications icons (for Icecat and others)

        ;; Spelling dictionaries
        aspell
        aspell-dict-en  ; English
        aspell-dict-ru  ; Russian

        translate-shell ; Translation in CLI and Emacs

        git             ; Version control
        magit           ; Emacs interface for Git

        icecat          ; Web browser

        isync           ; Sync IMAP

        feh             ; Image viewer
        mpv             ; Video and audio player

        qemu            ; Virtualization

        rdiff-backup    ; Incremental backups

        ;; Encryption and signing
        gnupg
        pinentry        ; Password typing for Gnupg

        password-store  ; Password management

        recutils        ; Filter records like in `guix --search'

        font-dejavu     ; General fonts
        font-liberation ; Times New Roman replcement
        fontconfig      ; Make `fc-cache -f' after installing addional fonts.

        pavucontrol     ; Pulseaudio control GUI
        pulsemixer      ; Pulseaudio control CLI

        sbcl   ; For StumpWM.  See <https://stumpwm.github.io/>.

        ;; For helm-stumpwm-commands and stumpish
        rlwrap
        xprop

        ;; $EDITOR
        emacs                    ; The best editor
        emacs-debbugs            ; <https://debbugs.gnu.org/> interface
        emacs-guix               ; Guix interface
        geiser                   ; Scheme bridge
        emacs-engine-mode        ; Define searches on websites
        emacs-projectile         ; Project functions
        emacs-helm-projectile    ; Helm interface for Projectile
        emacs-helm               ; Narrowing framework
        emacs-yasnippet          ; Snippets
        emacs-yasnippet-snippets
        emacs-company            ; Complition framework
        emacs-company-quickhelp  ; Help pages for Company
        emacs-smartparens        ; Structured editing
        emacs-transpose-frame    ; M-x transpose-frame
        emacs-multiple-cursors   ; Multi cursor
        emacs-gitpatch           ; Send patches

        ;; X11 utils.  See <~/.xsession> and <~/.stumpwm.d/init.lisp>.
        setxkbmap  ; Keyboard layout
        xclip      ; Access clipboard from CLI
        xrdb
        xset
        xsetroot
        xterm      ; $TERM

        openssh    ; SSH

        screen     ; Terminal multiplexer

        redshift   ; Eyes saver.

        ;; Downloaders.
        youtube-dl ; Video and music from websites
        wget))

(packages->manifest %desktop-packages)
