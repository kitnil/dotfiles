;; Add my personal packages to those Guix provides.
(cons* (channel
        (name 'guix-wigust)
        (url "https://cgit.duckdns.org/git/guix/guix-wigust"))
       (channel
        (name 'guix-wigust-services)
        (url "https://cgit.duckdns.org/git/guix/guix-wigust-services"))
       (channel
        (name 'guix-majordomo)
        (url "https://cgit.duckdns.org/git/guix/guix-majordomo"))
       (channel
        (name 'guix-tome4)
        (url "https://cgit.duckdns.org/git/guix/guix-tome4"))
       %default-channels)
