(list (channel
        (name 'guix)
        (url "https://github.com/kitnil/guix")
        (branch "custom")
        (commit
          "ca48476418ebc1bcb68b5544b4487755a633d42e")
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
          "7cb8244b9f1740bad72c2a9fb2bbf465c45e7d0e"))
      (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (branch "master")
        (commit
          "ec7c7b852c024095e4f34523452230406a3b4549")
        (introduction
          (make-channel-introduction
            "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
            (openpgp-fingerprint
              "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))))
