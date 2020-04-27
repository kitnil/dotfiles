(use-modules (guix channels)
             (guix inferior)
             (guix profiles)
             (srfi srfi-1))

(define channels
  (list (channel
         (name 'guix)
         (url "https://cgit.duckdns.org/git/guix/guix")
         (commit
          "cc73d8f47e20e4bf05f8f3147a2f20e0e3e46aab"))))

(define inferior
  (inferior-for-channels channels))

(packages->manifest (append (lookup-inferior-packages inferior "deepdiff")
                            (lookup-inferior-packages inferior "kodi")))
