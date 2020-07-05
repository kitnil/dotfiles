;; Add my personal packages to those Guix provides.
;;
;; WARNING: Don't use commits because of bin/executable_dotfiles
(list (channel
       (name 'guix-wigust)
       (url "https://github.com/kitnil/guix-wigust"))
      (channel
       (name 'guix-wigust-services)
       (url "https://github.com/kitnil/guix-wigust-services"))
      (channel
       (name 'guix-majordomo)
       (url "https://gitlab.com/wigust/guix-majordomo"))
      (channel
       (name 'guix-tome4)
       (url "https://github.com/kitnil/guix-tome4"))
      (channel
       (name 'guix)
       (url "https://github.com/kitnil/guix")
       (branch "wip-local")))
