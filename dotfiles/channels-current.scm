(list (channel
        (name 'guix)
        (url "https://git.savannah.gnu.org/git/guix.git")
        (branch "master")
        (commit
          "b53fac227836bcec3a2a7a44f7720b9a23db90f2")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
      (channel
        (name 'guix-wigust)
        (url "https://github.com/kitnil/guix-wigust")
        (branch "master")
        (commit
          "83e86a2891dd57f54fc3568d6a56581fabbb02d2"))
      (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (branch "master")
        (commit
          "34935b49ae20d236b0fc89943f0b1d0d5dea3a33")
        (introduction
          (make-channel-introduction
            "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
            (openpgp-fingerprint
              "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      (channel
        (name 'prometheus-shepherd-exporter)
        (url "https://gitlab.com/wigust/prometheus-shepherd-exporter")
        (branch "master")
        (commit
          "542ec52c4955c854e770f615148ced99de5e9fec")))
