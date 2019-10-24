(use-modules (guix channels)
             (guix inferior)
             (srfi srfi-1))

(define channels
  (list (channel
         (name 'guix)
         (url "https://git.savannah.gnu.org/git/guix.git")
         (branch "version-1.0.1"))
        (channel
         (name 'guix-majordomo)
         (url "file:///home/oleg/src/guix-majordomo")
         (commit
          "276f1d2aa0de76a47139b207e6d1af262cb5fd51"))))

(define inferior
  (inferior-for-channels channels))

(packages->manifest (lookup-inferior-packages inferior "guile-ihs"))
