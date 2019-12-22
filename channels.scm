;; Add my personal packages to those Guix provides.
(cons* (channel
        (name 'guix-wigust)
        (url "https://cgit.duckdns.org/git/guix/guix-wigust"))
       (channel
        (name 'guix-wigust-services)
        (url "file:///srv/git/guix/guix-wigust-services.git"))
       %default-channels)
