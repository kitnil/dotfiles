;; This "manifest" file can be passed to 'guix package -m' to reproduce
;; the content of your profile.  This is "symbolic": it only specifies
;; package names.  To reproduce the exact same profile, you also need to
;; capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules (gnu packages))

(specifications->manifest
 (append (list "socat"
               "mtr"
               "pulsemixer"
               "wl-clipboard"
               "translate-shell"
               "curl"
               "autojump"
               "tigervnc-client"
               "lm-sensors"
               "fd"
               "htop"
               "screen"
               "iputils"
               "make"
               "tmux"
               "gnupg"
               "password-store"
               "pavucontrol"
               "strace"
               "rsync"
               "dmenu"
               "ripgrep"
               "git"
               "ncdu"
               "lm-sensors"
               "python"
               "python-psutil"
               "mpv"
               "gron"
               "jq"
               "bluez"
               "moonlight-qt"
               "v4l-utils"
               "dialog"
               "pinentry"
               "sshpass"
               "freerdp"
               "adwaita-icon-theme"
               "remmina"
               "looking-glass-client"
               "netcat-openbsd"
               "clipman"
               "qbittorrent"
               "direnv"
               "skopeo")))
