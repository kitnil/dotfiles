(use-modules (gnu)
             (guix packages)
             (guix profiles))

(use-package-modules admin algebra aspell audio backup bittorrent
cdrom ci cmake code commencement compression conkeror cpio cran
databases dictionaries dns elf emacs games gcc gdb ghostscript gl glib
gnu-doc gnupg gnuzilla graphics graphviz gstreamer gtk guile haskell
image-viewers imagemagick inkscape kodi libreoffice license linux lisp
logging lsof lxde m4 mail man maths messaging ncdu ncurses networking
node package-management parallel password-utils patchutils pdf perl
perl-web python rdesktop samba scheme screen shellutils ssh statistics
suckless synergy tex texinfo textutils tls tor valgrind
version-control video virtualization w3m web xdisorg xml xorg)

(define %emacs-packages
  (list emacs-academic-phrases
        emacs-ace-window
        emacs-add-hooks
        emacs-ag
        emacs-aggressive-indent  ; Auto indent minor mode
        emacs-auto-yasnippet
        emacs-bash-completion
        emacs-bbdb
        emacs-beginend
        emacs-browse-at-remote
        emacs-build-farm
        emacs-cmake-mode
        emacs-company            ; Complition framework
        emacs-company-quickhelp  ; Help pages for Company
        emacs-constants
        emacs-crux
        emacs-debbugs ; <https://debbugs.gnu.org/> interface
        emacs-default-encrypt    ; Sign mail automatically
        emacs-default-text-scale ; Scale text in all buffers
        emacs-dumb-jump
        emacs-edit-server ; See <https://github.com/stsquad/emacs_chrome/>.
        emacs-elfeed             ; RSS reader
        emacs-emms
        emacs-erc-hl-nicks       ; for ERC
        emacs-esup
        emacs-ewmctrl            ; Control X windows from Emacs
        emacs-expand-region
        emacs-f3
        emacs-fancy-narrow
        emacs-ffap-rfc-space
        emacs-fill-column-indicator
        emacs-flycheck                 ; Syntax checker
        emacs-geiser                   ; Scheme bridge
        emacs-ggtags             ; Front end to GNU Global
        emacs-git-auto-commit-mode
        emacs-git-gutter
        emacs-git-timemachine
        emacs-gitpatch           ; Send patches
        emacs-go-mode
        emacs-god-mode           ; Commands without modifier keys
        emacs-grep-context
        emacs-guix
        emacs-helm               ; Narrowing framework
        emacs-helm-eww
        emacs-helm-firefox       ; Search for bookmarks in Icecat
        emacs-helm-gtags
        emacs-helm-make          ; Front end to `make'
        emacs-helm-mode-manager
        emacs-helm-projectile    ; Helm interface for Projectile
        emacs-helm-shell-history
        emacs-highlight-sexp
        emacs-highlight-stages   ; Highlight code stages
        emacs-highlight-symbol
        emacs-hl-todo
        emacs-htmlize
        emacs-hy-mode
        emacs-hydra
        emacs-ibuffer-projectile
        emacs-irfc
        emacs-ivy                ; Complition framework
        emacs-know-your-http-well
        emacs-lice-el
        emacs-lua-mode
        emacs-macrostep
        emacs-magit
        emacs-magit-org-todos-el
        emacs-markdown-mode      ; Commonmark major mode
        emacs-mbsync
        emacs-move-text
        emacs-multiple-cursors   ; Multi cursor
        emacs-nginx-mode
        emacs-nix-mode           ; Nix language mode
        emacs-org                ; Org
        emacs-org-bullets
        emacs-org-mind-map       ; General mind maps from Org files
        emacs-outshine           ; Emacs outline-mode
        emacs-php-mode
        emacs-projectile         ; Project functions
        emacs-rainbow-delimiters ; Prettify parentheses
        emacs-rainbow-mode       ; Show colors in codes
        emacs-restclient
        emacs-scheme-complete
        emacs-slime              ; Sbcl repl
        emacs-smart-mode-line
        emacs-smartparens        ; Structured editing
        emacs-string-inflection
        emacs-suggest
        emacs-transmission       ; Front end to transmission-daemon
        emacs-transpose-frame    ; M-x transpose-frame
        emacs-undo-tree          ; Undo visualisation
        emacs-use-package        ; Lazy configuration
        emacs-validate
        emacs-w3m                ; Front end to w3m command line web browser
        emacs-which-key          ; Key bindings help
        emacs-yaml-mode          ; YAML files
        emacs-yasnippet          ; Snippets
        emacs-yasnippet-snippets ; Collection of snippets
        ))

(define guix-collection-packages-multiout
  `((,glib "bin")
    (,git "gui")
    (,git "send-email")
    (,git "svn")
    (,alsa-plugins "pulseaudio")
    (,isc-bind "utils")))

(define %large-packages
  (list libreoffice))

(define %spelling-packages
  (list aspell aspell-dict-en aspell-dict-ru))

(packages->manifest
 (append (list feh         ; Image viewer
               ffmpeg      ; Video, audio, images, gif conversion
               imagemagick ; Pipe to `display'

               ;; See <https://github.com/NixOS/nixpkgs/issues/16327#issuecomment-303068424>.
               at-spi2-core
               ghostscript/x
               ghc-pandoc  ; Convert Markdown
               gnuplot
               jq

               cloc            ; Count code
               direnv
               epipe
               gnu-make        ; GNU Make
               recutils  ; Filter records like in `guix --search'
               stow            ; Dotfiles management
               the-silver-searcher
               woof

               translate-shell ; Translation in CLI and Emacs
               password-store  ; Password management

               python-hy

               cuirass

               guile-2.2
               guile-colorized
               guile-daemon
               guile-fibers
               guile-gcrypt
               guile-git
               guile-readline
               guile-ssh
               guile-xosd

               mcron

               colordiff
               colormake
               perl

               git       ; Version control


               guile-commonmark ; Commonmark for Guile
               gwl              ; Guix workflow management
               haunt            ; Guile static site generator

               aria2        ; Download utility
               kodi-cli     ; Remote control Kodi
               transmission ; Bittorrent

               mps-youtube
               streamlink
               twitchy
               youtube-dl   ; Video and music from websites

               redshift  ; Color temperature
               python-clf ; Interface to <https://commandlinefu.com/>
               neofetch

               lxappearance

               alsa-utils
               cli-visualizer

               icecat ; Web browser

               node ;Packages in <~/.npm-global/bin/>.

               torsocks
               tor

               isync   ; Sync IMAP
               msmtp
               notmuch ; Mail indexer based on Xapian

               inkscape    ; Vector graphics
               mlt         ; Video editing framework
               mpv         ; Video and audio player
               obs ;ffmpeg frontend
               vlc

               zathura     ; Lightweight customizable PDF reader
               zathura-djvu
               zathura-pdf-mupdf

               keynav
               rofi
               scrot ;screenshot
               st
               xev
               xsel ;clipboard

               perl-uri-escape ;convert url

               patchelf ;patch elf

               octave

               hdparm
               htop            ; Pretty `top'
               inxi
               iotop
               jnettop
               python-glances
               multitail

               synergy

               ;; FAIL: ansible         ; Configuration management
               bc
               cpio
               detox           ; Replace spaces with underscores in filenames
               diffoscope
               dos2unix
               freerdp
               graphviz  ; `dot'
               html-xml-utils
               licensecheck ; Licence checker for source files
               lsof
               ncdu            ; TUI `du'
               netcat          ; TCP
               nmap
               openssl
               parallel
               reptyr
               shellcheck
               socat
               sqlite
               unzip
               wireshark
               zip

               texinfo


               cdrkit-libre
               virt-manager
               qemu
               samba

               emacs-znc
               znc

               gnu-c-manual ; C language documentation
               gnu-standards
               man-pages
               sicp         ; Structure and Interpretation of Computer Programs
               texlive

               chicken ; Chicken Scheme
               sbcl ; For StumpWM.  See <https://stumpwm.github.io/>.
               sbcl-stumpwm

               python-pygments ; Colorize output
               w3m

               restic ; Incremental backup

               dbus

               cflow         ;C program call map.
               gcc-toolchain ;For Emacs `semantic-mode'.
               gdb           ;GNU debuger.
               global        ;Source tagging.
               valgrind      ;Memory debug.

               gource

               mercurial

               mesa-utils
               mesa

               gst-plugins-base
               gst-plugins-bad
               gst-plugins-good
               gst-plugins-ugly
               gstreamer

               minetest                 ; Open source Minecraft

               m4

               r)

         %emacs-packages
         %large-packages
         %spelling-packages))
