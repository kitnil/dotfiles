(define-module (wigust-packages)
  #:use-module (wigust packages emacs)
  #:use-module (wigust packages gource)
  #:use-module (wigust packages pulseaudio)
  #:use-module (wigust packages python))

(define guix-wigust-packages
  (list
   streamlink   ; `streamlink -p mpv URL QUALITY'

   pulsemixer-emacs-keybindings
   emacs-athena
   emacs-beginend
   emacs-engine-mode-autoload ; Define searches on websites
   emacs-strace-mode-special ; Colorize `strace' logs

   ;; See <https://github.com/rmuslimov/browse-at-remote/pull/46>.
   emacs-browse-at-remote-gnu

   emacs-edit-server ; See <https://github.com/stsquad/emacs_chrome/>.

   raleigh-reloaded-theme ; GTK2+ and GTK3+ theme

   licensecheck ; Licence checker for source files

   python-starred ; Fetch a list of stars from GitHub user

   emacs-eval-in-repl       ; Evaluate to different Repls
   emacs-add-hooks
   emacs-crux
   emacs-debpaste           ; Front end to <https://paste.debian.net/>
   emacs-esup
   emacs-ewmctrl            ; Control X windows from Emacs
   emacs-fancy-narrow
   emacs-helm-firefox       ; Search for bookmarks in Icecat
   emacs-helm-gtags
   emacs-debbugs-with-bugs  ; <https://debbugs.gnu.org/> interface
   emacs-default-text-scale ; Scale text in all buffers
   emacs-guix-checkout      ; Guix interface
   emacs-helm-emms
   epipe
   emacs-helm-mode-manager
   emacs-helm-pass          ; Front end to password-store
   emacs-move-text
   emacs-org-mind-map       ; General mind maps from Org files
   emacs-helm-c-yasnippet
   emacs-terminal-here
   gource-good-hash ; 3D visualisation tool for source control repositories
   ))
