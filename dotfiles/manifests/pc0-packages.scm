;; This "manifest" file can be passed to 'guix package -m' to reproduce
;; the content of your profile.  This is "symbolic": it only specifies
;; package names.  To reproduce the exact same profile, you also need to
;; capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(specifications->manifest
  (list "socat"
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
        "firefox"
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
        "sshpass"))
