(defun wi-set-current-frame-80-40 ()
  "Set current frame to 80 pixels width and 40 pixels height."
  (interactive)
  (set-frame-size (selected-frame) 80 40))

(defun wi-set-current-frame-80-24 ()
  "Set current frame to 80 pixels width and 24 pixels height."
  (interactive)
  (set-frame-size (selected-frame) 80 24))

;; Origin https://emacsredux.com/blog/2020/07/18/automatically-kill-running-processes-on-exit/
(setq confirm-kill-processes nil)

;; Open buffer in vertical split by default
(setq split-height-threshold nil)
(setq split-width-threshold 160)

;; Origin <https://github.com/Wilfred/.emacs.d/blob/gh-pages/init.org>.
(setq scroll-preserve-screen-position 'always) ; Preserve scroll pos.
(setq dumb-jump-max-find-time 4)

;; Origin <https://github.com/Wilfred/.emacs.d/blob/gh-pages/init.org>.
(setq enable-recursive-minibuffers t) ; Enable recursive minibuffer.

;; (with-eval-after-load 'tramp
;;   (recentf-mode 1))
(setq recentf-max-menu-items 30)
(setq recentf-max-saved-items nil)
(setq recentf-auto-cleanup 'never)

;; See <https://www.emacswiki.org/emacs/DoWhatIMean>
(setq dired-dwim-target t)

(add-hook 'diff-mode-hook (lambda () (setq-local truncate-lines t)))

(setq dumb-jump-selector 'ivy)
(setq dumb-jump-force-searcher 'ag)

(autoload 'crux-transpose-windows "crux" nil t)
(autoload 'crux-open-with "crux" nil t)

(autoload 'stupid-indent-mode "stupid-indent-mode" nil t)

;; (global-undo-tree-mode)
;; (setq undo-tree-auto-save-history t)
;; (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;; Default from Emacs 26
;; See <http://git.savannah.gnu.org/cgit/emacs.git/commit/etc/NEWS?id=72ee93d68daea00e2ee69417afd4e31b3145a9fa>
(setq print-quoted t)

;; Enable functions
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

(setq-default fill-column 78)

(defun wi-switch-to-scratch-elisp ()
  "Switch to *scratch* buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(setq mouse-yank-at-point t) ; Ignore mouse position on paste
(setq mouse-autoselect-window t) ; Sloppy mouse

 ; Unprettify symbol after the cursor
(setq prettify-symbols-unprettify-at-point 'right-edge)

(setq whitespace-style
      '(face tabs
             spaces
             trailing
             ;; lines
             space-before-tab
             newline
             indentation
             empty
             space-after-tab
             space-mark
             tab-mark
             ;; newline-mark
             ))

(when (boundp #'hl-todo-mode)
  (add-hook 'prog-mode-hook 'hl-todo-mode))

;; (add-hook 'prog-mode-hook 'which-function-mode)

(when (functionp #'symbol-overlay-mode)
  (add-hook 'prog-mode-hook 'symbol-overlay-mode))

(when (functionp #'hideshowvis-minor-mode)
  (add-hook 'prog-mode-hook 'hideshowvis-minor-mode)
  (add-hook 'conf-javaprop-mode-hook 'hideshowvis-minor-mode))

(setq wi-groups-direcotories-git t)
(setq wi-groups-direcotories
      '("~/src/gitlab.intr"
        "~/src/anongit.gentoo.org"
        "~/src/ansible-cmdb-majordomo"
        "~/src/ansible-ping"
        "~/src/bento"
        "~/src/bitbucket.org"
        "~/src/cc"
        "~/src/cerb-js"
        "~/src/cgit.duckdns.org"
        "~/src/code.orgmode.org"
        "~/src/discord"
        "~/src/docker-nextcloud"
        "~/src/docker-stack-deploy-pipeline"
        "~/src/docker-tftp"
        "~/src/dwl"
        "~/src/elk"
        "~/src/elpa"
        "~/src/emacs-anywhere-mode"
        "~/src/emacs-guix-misc"
        "~/src/emacs-hydra-timestamp"
        "~/src/emacs-redshift"
        "~/src/emacs-wi-tabu-youtube"
        "~/src/emacs-wi-utils"
        "~/src/erza"
        "~/src/example-nodejs-app"
        "~/src/files-maintenance"
        "~/src/gesturefy"
        "~/src/git.archlinux.org"
        "~/src/git.dn42.dev"
        "~/src/github.com"
        "~/src/git.ispconfig.org"
        "~/src/git.kernel.org"
        "~/src/gitlab"
        "~/src/gitlab.com"
        "~/src/gitlab.intr"
        "~/src/gitlab.wugi.info"
        "~/src/git.net-core.org"
        "~/src/gitolite-admin"
        "~/src/git.php.net"
        "~/src/git.savannah.gnu.org"
        "~/src/git.savannah.nongnu.org"
        "~/src/git.slackbuilds.org"
        "~/src/git.sr.ht"
        "~/src/git.zabbix.com"
        "~/src/grafana"
        "~/src/guile-dovecot"
        "~/src/guile-feed"
        "~/src/guile-haunt"
        "~/src/guile-hello-rogue"
        "~/src/guile-remote"
        "~/src/guix-browse"
        "~/src/guix-eve-online"
        "~/src/guix-gtk"
        "~/src/guix-latest-eval"
        "~/src/guix-media"
        "~/src/guix-media-nonfree"
        "~/src/guix-wigust-workflow"
        "~/src/hello-agda"
        "~/src/hello-alerta"
        "~/src/hello-assembly"
        "~/src/hello-bash"
        "~/src/hello-c"
        "~/src/hello-cepl"
        "~/src/hello-chicken"
        "~/src/hello-clojure"
        "~/src/hello-coq"
        "~/src/hello-coredump"
        "~/src/hello-cpp"
        "~/src/hello-cuirass"
        "~/src/hello-gnu-accembly"
        "~/src/hello-go"
        "~/src/hello-godot"
        "~/src/hello-guile"
        "~/src/hello-gwl"
        "~/src/hello-haskell"
        "~/src/hello-karate"
        "~/src/hello-lisp"
        "~/src/hello-middleman"
        "~/src/hello-nightmarejs"
        "~/src/hello-nix"
        "~/src/hello-ocaml"
        "~/src/hello-php"
        "~/src/hello-polymer-app"
        "~/src/hello-rust"
        "~/src/hello-scheme"
        "~/src/hello-sdl"
        "~/src/hello-spring"
        "~/src/hello-zabbix"
        "~/src/home"
        "~/src/kube"
        "~/src/ledger"
        "~/src/lua"
        "~/src/majordomo-backup"
        "~/src/majordomo-help"
        "~/src/majordomo-maintenance"
        "~/src/majordomo-src"
        "~/src/majordomo-src-fool"
        "~/src/majordomo-tickets"
        "~/src/malware-scanner"
        "~/src/math"
        "~/src/monitoror"
        "~/src/mpv-scripts"
        "~/src/nixos-mailserver-flake"
        "~/src/nixpkgs-wigust"
        "~/src/noise"
        "~/src/opensearch"
        "~/src/paws"
        "~/src/pelzflorian.de"
        "~/src/python-nmap2json"
        "~/src/remote"
        "~/src/ssh-exporter"
        "~/src/ssl"
        "~/src/sup2eng"
        "~/src/tab-slideshow"
        "~/src/temp"
        "~/src/terraform-gitlab"
        "~/src/terraform-notabug"
        "~/src/tick"
        "~/src/tmp"
        "~/src/tome4-notes"
        "~/src/upwork-telethon"
        "~/src/wigust.github.io"
        "~/src/wigust-resume"
        "~/src/wireguard"
        "~/src/zabbix"
        "~/src/zabbix-aws"
        "~/src/zabbix-guix"))
(setq wi-projects-directories '("~/src" "~/archive/src"))

(defun wi-find-file-readlink ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (kill-buffer (current-buffer))
    (find-file
     (string-trim
      (shell-command-to-string
       (format "readlink -f %s" file-name))))))

(add-hook 'find-file-hook
          '(lambda ()
             (when (string-match (rx (and any ".guix-profile")) (buffer-file-name))
               (wi-find-file-readlink))))

(setq auto-insert-directory
      (expand-file-name "~/.emacs.d/insert"))

(defalias 'center-mode 'olivetti-mode)

;; Show recursion depth.
(progn
  (minibuffer-depth-indicate-mode)

  (when (boundp #'global-git-gutter-mode)
    (global-git-gutter-mode))

  (when (boundp #'default-text-scale-mode)
    (default-text-scale-mode))

  (when (boundp #'projectile-mode)
    (projectile-global-mode))

  ;; Don't use ido
  (setq projectile-completion-system 'ivy)

  (when (boundp #'projectile-project-root-files)
    (add-to-list 'projectile-project-root-files "environment-variables"))

  (setq projectile-switch-project-action 'projectile-dired)

  (when (boundp #'projectile-mode-map)
    (define-key projectile-mode-map (kbd "C-c g p") 'projectile-command-map)
    (define-key projectile-mode-map (kbd "C-c g p h") 'helm-projectile))

  (when (boundp #'beginend-global-mode)
    (beginend-global-mode))
  (save-place-mode)            ; Remember position in files

  ;; Toggle prettify symbols mode on
  (global-prettify-symbols-mode)

  ;; Undo and redo operations on windows and buffers
  (winner-mode 1)
  ;; (windmove-default-keybindings) ;XXX: Breaks vterm-mode-map

  ;; Display key bindings help window (after some delay)
  (when (boundp #'which-key-mode)
    (which-key-mode))

  ;; Toggle show-paren-mode on
  (show-paren-mode))

;; Emacs redraw issue in X.org on VMWare and VirtualBox | fujii.github.io
;; <https://fujii.github.io/2016/09/06/emacs-redisplay-issue-on-vmware/>
(when (eq window-system 'x)
  (add-hook 'window-scroll-functions
            (lambda (&rest x)
              (run-with-idle-timer 0.5 nil 'redraw-display))))
