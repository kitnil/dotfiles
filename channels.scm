(list (channel
        (name 'guix)
        (url "https://git.savannah.gnu.org/git/guix.git")
        (commit
         "b5e912a88cd57108ac12bd04a28b9e17e4a46e86"))
      (channel
       (name 'guix-wigust)
       (url "https://gitlab.wugi.info/guix/guix-wigust.git")
       (branch "@CI_COMMIT_REF_NAME@")))
