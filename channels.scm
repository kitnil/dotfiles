(list (channel
        (name 'guix)
        (url "https://git.savannah.gnu.org/git/guix.git")
        (commit
         "24446ce299943efe3dfded6c9dd0cf9421d8da04"))
      (channel
       (name 'guix-wigust)
       (url "@CI_PROJECT_URL@")
       (branch "@CI_COMMIT_REF_NAME@")))
