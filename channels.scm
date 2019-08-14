(list (channel
        (name 'guix)
        (url "https://git.savannah.gnu.org/git/guix.git")
        (commit
         "4afdb792497472b968d818a3ed942beb70210389"))
      (channel
       (name 'guix-wigust)
       (url "@CI_PROJECT_URL@")
       (branch "@CI_COMMIT_REF_NAME@")))
