(use-modules (guix channels)
             (guix inferior)
             (guix profiles)
             (srfi srfi-1))

(define channels
  (list (channel
         (name 'guix)
         (url "https://cgit.duckdns.org/git/guix/guix")
         (commit
          "ee4db149c29b6d0fe6bcab552f163bbbe5b09738"))))

(define inferior
  (inferior-for-channels channels))

(first (lookup-inferior-packages inferior "php"))
