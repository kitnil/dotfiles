;; This "manifest" file can be passed to 'guix package -m' to reproduce
;; the content of your profile.  This is "symbolic": it only specifies
;; package names.  To reproduce the exact same profile, you also need to
;; capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(specifications->manifest
  (list "socat"
        "mtr"
        "pulsemixer"
        "virt-manager-fullscreen"
        "emacs-nix-mode"
        "emacs-guix"
        "wl-clipboard"
        "emacs-add-hooks"
        "emacs-smartparens"
        "translate-shell"
        "curl"
        "autojump"
        "emacs-aggressive-indent"
        "tigervnc-client"
        "emacs-helm"
        "emacs-helm-projectile"
        "emacs-ivy"
        "emacs-projectile"
        "emacs-deadgrep"
        "emacs-default-text-scale"
        "lm-sensors"
        "fd"
        "htop"
        "tmux"
        "gnupg"
        "emacs-use-package"
        "password-store"
        "emacs-magit"
        "pavucontrol"
        "strace"
        "emacs-next-pgtk"
        "rsync"
        "firefox"
        "dmenu"
        "ripgrep"
        "git"
        "ncdu"))
