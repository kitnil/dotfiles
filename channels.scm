(list (channel
        (name 'guix)
        (url "https://git.savannah.gnu.org/git/guix.git")
        (commit
         "ee6bfffe1984c63070e5c9510722b586ccd4b35d"))
      (channel
       (name 'guix-wigust)
       (url "@CI_PROJECT_URL@")
       (branch "@CI_COMMIT_REF_NAME@")))
