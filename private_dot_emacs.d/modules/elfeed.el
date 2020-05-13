(setq elfeed-feeds
      '("https://oremacs.com/atom.xml"
        "http://steckerhalter.tk/index.xml"
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
        ("https://news-web.php.net/group.php?group=php.announce&format=rss" php)
        ("https://www.bennee.com/~alex/blog/feed/" emacs)
        ("https://libraries.io/search.atom?order=desc&platforms=Emacs&sort=created_at" emacs)
        ("https://cestlaz.github.io/rss.xml" emacs)
        ("https://sachachua.com/blog/feed/" emacs)
        ("https://www.reddit.com/r/freegames/.rss" game)
        ("https://archlinux.org/feeds/planet" planet)
        ("https://weekly.nixos.org/feeds/all.rss.xml" planet)
        ("http://www.scheme.dk/planet/atom.xml" planet)
        ("http://planet.gnu.org/atom.xml" planet)
        ("http://planet.lisp.org/rss20.xml" planet)
        ("https://planet.emacslife.com/atom.xml" planet)
        ("https://twitchrss.appspot.com/vod/artgameslp" game video)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC7YOGHUfC1Tb6E4pudI9STA" video) ;Mental Outlaw
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCq2QwnKW79w4a55ZQqOBReg" video) ;Irishluck Linux
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCF3d6ZcTRBhnrNC0-cvzicw" video)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC1RZz5_cdVQHhhYJVpCDqHA" video)
        ("https://www.youtube.com/feeds/videos.xml?user=elementaller" video)
        ("https://www.youtube.com/feeds/videos.xml?user=Pashtet495" video)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCOpm7EqPBtznEwYNNZrz1FQ" video)
        ("https://www.youtube.com/feeds/videos.xml?user=gotbletu" video)
        ("https://www.youtube.com/feeds/videos.xml?user=SsethTzeentach" video)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA" video)
        ("https://www.youtube.com/feeds/videos.xml?user=metalx1000" video)
        ("https://www.youtube.com/user/OmegaDungeon" video)
        ("https://bitlove.org/jupiterbroadcasting/bsdnowhd/feed" video)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCld68syR8Wi-GY_n4CaoJGA" video) ;Brodie Robertson
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCVls1GmFKf6WlTraIb_IaJg" video)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCkK9UDm_ZNrq_rIXCz3xCGA" video)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCbHXJGd7c8Hy4z0-YX1Jf3Q" video)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCVls1GmFKf6WlTraIb_IaJg" video)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCMV8p6Lb-bd6UZtTc_QD4zA" video)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCZrrEuHiQjN2CUo84g5tk7w" video)
        ("https://www.youtube.com/feeds/videos.xml?user=dubbeltumme" video)
        ("https://www.youtube.com/feeds/videos.xml?user=LDCNow" video)
        ("https://www.youtube.com/feeds/videos.xml?user=tuxreviews" video)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCgU5tUdVPpfM7sLAMWBTsDg" video)))

(with-eval-after-load 'elfeed
  (defun wi-elfeed-search-show-entry ()
    "Call `elfeed-search-show-entry' with `shr-width' setted to NIL."
    (interactive)
    (let ((shr-width nil))
      (call-interactively 'elfeed-search-show-entry)))
  (let ((map elfeed-search-mode-map))
    (define-key map (kbd "RET") 'wi-elfeed-search-show-entry)
    (define-key map (kbd "SPC") 'scroll-up-command)
    (define-key map (kbd "S-SPC") 'scroll-down-command)))

(run-at-time nil (* 60 10)
             #'(lambda ()
                 (let ((time (current-idle-time)))
                   (when (and time (> (time-to-seconds time) (* 60 5)))
                     (elfeed-update)))))
