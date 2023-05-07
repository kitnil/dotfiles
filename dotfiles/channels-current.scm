(list (channel
        (name 'guix)
        (url "https://git.savannah.gnu.org/git/guix.git")
        (branch "master")
        (commit
          "1f734a6f0a7db5b0e12091a0c869c5c4810ac80e")
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
          "416882513017c139918fb42548634d07d565cc74"))
      (channel
       (name 'prometheus-shepherd-exporter)
       (url "https://gitlab.com/wigust/prometheus-shepherd-exporter")
       (branch "master")
       (commit
         "542ec52c4955c854e770f615148ced99de5e9fec"))
      (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (branch "master")
        (commit
          "3f00d57adce5d0a185708fd5c7c5ff6f852c2bf7")
        (introduction
          (make-channel-introduction
            "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
            (openpgp-fingerprint
              "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))))
