(list (channel
        (name 'guix)
        (url "https://cgit.wugi.info/git/guix/guix")
        (branch "master")
        (commit
          "32bd53cdb28cf35310f9067d4450e0113071a900")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
      (channel
        (name 'guix-wigust)
        (url "https://cgit.wugi.info/git/guix/guix-wigust")
        (branch "master")
        (commit
          "91a9f9dd8bb9a7c356ddedebf06d20a3f9e84258"))
      (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (branch "master")
        (commit
          "9446bf27a40a429baf98248ffa5d1b6942ac7f16")
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
