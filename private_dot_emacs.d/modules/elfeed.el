;; -*- eval: (goto-address-mode 1); -*-

(require 'guix-ui-package) ;for guix-show-emacs-package-without-prefix

(setq elfeed-feeds
      '("https://oremacs.com/atom.xml"
        "http://nullprogram.com/feed/"
        "https://h-node.org/rss/modifications/en"
        "https://lwn.net/headlines/newrss"
        "https://fedoramagazine.org/feed/"
        "https://www.youtube.com/feeds/videos.xml?channel_id=UC_ehNByPcItZU3pXL-4skUA"
        "https://twitchrss.appspot.com/vod/carolinhr"
        ("https://news.ycombinator.com/rss" news)
        ("http://government.ru/all/rss/" gov ru)
        ("https://git.savannah.gnu.org/cgit/emacs.git/atom/?h=master" vc emacs)
        ("https://git.savannah.gnu.org/cgit/guix.git/atom/?h=master" vc scheme guix)
        ("https://github.com/stumpwm/stumpwm-contrib/commits/master.atom" vc lisp stumpwm)
        ("https://github.com/stumpwm/stumpwm/commits/master.atom" vc lisp stumpwm)
        ("https://github.com/alebcay/awesome-shell/commits/master.atom" vc bash awesome)
        ("https://github.com/adelin-b/yawhich-key/commits/master.atom" vc bash)
        ("https://github.com/sindresorhus/awesome/commits/main.atom" vc awesome)
        ("https://news-web.php.net/group.php?group=php.announce&format=rss" php)
        ("https://www.bennee.com/~alex/blog/feed/" emacs)
        ("https://libraries.io/search.atom?order=desc&platforms=Emacs&sort=created_at" emacs)
        ;; ("https://melpa.org/updates.rss" emacs)
        ("https://stable.melpa.org/updates.rss" emacs)
        ("https://cestlaz.github.io/rss.xml" emacs)
        ("https://sachachua.com/blog/feed/" emacs)
        ("https://www.reddit.com/r/freegames/.rss" game)
        ("https://www.reddit.com/r/selfhosted/.rss" reddit selfhosted)
        ("https://github.com/awesome-selfhosted/awesome-selfhosted/commits/master.atom" vc awesome selfhosted)
        ("https://github.com/sindresorhus/awesome/commits/main.atom" vc awesome)
        ("https://github.com/CodyReichert/awesome-cl/commits/master.atom" vc awesome lisp)
        ("https://archlinux.org/feeds/planet" planet)
        ("https://weekly.nixos.org/feeds/all.rss.xml" planet)
        ("http://www.scheme.dk/planet/atom.xml" planet)
        ("http://planet.gnu.org/atom.xml" planet)
        ("http://planet.lisp.org/rss20.xml" planet)
        ("https://planet.emacslife.com/atom.xml" planet)
        ("https://twitchrss.appspot.com/vod/artgameslp" game video)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC7YOGHUfC1Tb6E4pudI9STA" video) ;Mental Outlaw
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCq2QwnKW79w4a55ZQqOBReg" video) ;Irishluck Linux
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCF3d6ZcTRBhnrNC0-cvzicw" video) ;Nitroxsenys
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC1RZz5_cdVQHhhYJVpCDqHA" video) ;Nitro Live
        ("https://www.youtube.com/feeds/videos.xml?user=elementaller" video)
        ("https://www.youtube.com/feeds/videos.xml?user=Pashtet495" video)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCOpm7EqPBtznEwYNNZrz1FQ" video) ;stalkash
        ("https://www.youtube.com/feeds/videos.xml?user=gotbletu" video)
        ("https://www.youtube.com/feeds/videos.xml?user=SsethTzeentach" video)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA" video) ;Luke Smith
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCld68syR8Wi-GY_n4CaoJGA" video) ;Brodie Robertson
        ("https://youtube.com/feeds/videos.xml?channel_id=UCfhSB16X9MXhzSFe_H7XbHg" video)     ;Bryan Jenks
        ("https://www.youtube.com/feeds/videos.xml?user=metalx1000" video)
        ("https://bitlove.org/jupiterbroadcasting/bsdnowhd/feed" video)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCVls1GmFKf6WlTraIb_IaJg" video) ;DistroTube
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCkK9UDm_ZNrq_rIXCz3xCGA" video) ;Bryan Lunduke
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCbHXJGd7c8Hy4z0-YX1Jf3Q" video) ;WP Website Guide
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCMV8p6Lb-bd6UZtTc_QD4zA" video) ;Baggers
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCZrrEuHiQjN2CUo84g5tk7w" video) ;tripcode!Q/7
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCgY050JAKtaew3IEgGW1qSQ" video) ;Unix way
        ("https://www.youtube.com/feeds/videos.xml?user=dubbeltumme" video)
        ("https://www.youtube.com/feeds/videos.xml?user=LDCNow" video)
        ("https://www.youtube.com/feeds/videos.xml?user=tuxreviews" video)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCgU5tUdVPpfM7sLAMWBTsDg" video) ;computeremotion.com

        ("https://toot.aquilenet.fr/@civodul.rss" mastodon) ;Ludovic Courtès
        ("https://functional.cafe/@ioa.rss" mastodon) ;ioanna

        ("https://peertube.su/feeds/videos.atom?videoChannelId=5888" video) ;The GNU Guy
        ))

(defun elfeed-config ()
  (interactive)
  (find-file
   (expand-file-name
    "~/.local/share/chezmoi/private_dot_emacs.d/modules/elfeed.el")))

(defun elfeed-guix-show ()
  (interactive)
  (shell-command
   (format "guix show %s"
           (cadr (split-string (elfeed-entry-title
                                (car (elfeed-search-selected)))
                               ": ")))))

(with-eval-after-load 'elfeed
  (defun wi-elfeed-search-show-entry ()
    "Call `elfeed-search-show-entry' with `shr-width' setted to NIL."
    (interactive)
    (let ((shr-width nil))
      (call-interactively 'elfeed-search-show-entry)))
  ;; (setq elfeed-search-remain-on-entry t)
  (setq elfeed-show-entry-switch 'display-buffer)
  (let ((map elfeed-search-mode-map))
    (define-key map (kbd "RET") 'wi-elfeed-search-show-entry)
    (define-key map (kbd "h") 'other-window)
    (define-key map (kbd "SPC") 'scroll-up-command)
    (define-key map (kbd "S-SPC") 'scroll-down-command)
    (define-key map (kbd "<f8>") 'elfeed-config)
    (define-key map (kbd "B") 'elfeed-guix-show)
    (define-key map (kbd "=") 'elfeed-score-map)
    (define-key map (kbd "<f7>") 'elfeed-show-guix-emacs-package)
    ;; https://www.reddit.com/r/emacs/comments/hq3r36/elfeed_configuration_to_display_in_next_window/
    ;; (define-key map (kbd "n")
    ;;   '(lambda ()
    ;;      (interactive)
    ;;      (next-line)
    ;;      (call-interactively 'elfeed-search-show-entry)))
    ;; (define-key map (kbd "p")
    ;;   '(lambda () (interactive)
    ;;      (previous-line)
    ;;      (call-interactively 'elfeed-search-show-entry)))
    )
  (let ((map elfeed-show-mode-map))
    (define-key map (kbd "h") 'other-window))
  (setq elfeed-search-title-max-width 120)
  (add-hook 'elfeed-show-mode-hook 'visual-line-mode)

  (setq elfeed-search-filter "@1-weeks-ago +unread")

  ;; score
  (setq elfeed-search-print-entry-function #'elfeed-score-print-entry)
  (elfeed-score-enable))

(run-at-time nil (* 60 10)
             #'(lambda ()
                 (let ((time (current-idle-time)))
                   (when (and time (> (time-to-seconds time) (* 60 5)))
                     (elfeed-update)))))


;;;
;;; org-capture
;;;

;; Thanks to http://heikkil.github.io/blog/2015/05/09/notes-from-elfeed-entries/

(defun elfeed-link-title (entry)
  "Copy the entry title and URL as org link to the clipboard."
  (let ((titlelink (org-make-link-string (elfeed-entry-link entry)
                                         (elfeed-entry-title entry))))
    (when titlelink
      (kill-new titlelink)
      (x-set-selection 'PRIMARY titlelink)
      (message "Yanked: %s" titlelink))))

(defun elfeed-show-link-title ()
  "Copy the current entry title and URL as org link to the clipboard."
  (interactive)
  (elfeed-link-title elfeed-show-entry))


;;;
;;; Guix
;;;

(defun guix-show-emacs-package-without-prefix (package-without-prefix)
  (guix-package-get-display
   nil 'name (concat "emacs-"
                     (substring package-without-prefix 0
                                (seq-position (string-to-list package-without-prefix)
                                              (string-to-char " "))))))

(defun elfeed-show-guix-emacs-package ()
  (interactive)
  (let* ((entry (elfeed-search-selected t))
         (title (elfeed-entry-title entry)))
    (elfeed-untag entry 'unread)
    (elfeed-search-update-entry entry)
    (unless (or elfeed-search-remain-on-entry (use-region-p))
      (forward-line))
    (guix-show-emacs-package-without-prefix title)
    (message title)))
