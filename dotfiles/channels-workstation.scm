(list (channel
        (name 'guix)
        (url "https://cgit.wugi.info/git/guix/guix")
        (branch "master")
        (commit
          "4bfeff64b6e3c85a77a79c29fdb434e5a76b5b28")
        (introduction
          (make-channel-introduction
            "9eb32716a99d7010dee9da70b0f8219ef2689a66"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
      (channel
        (name 'guix-wigust)
        (url "https://cgit.wugi.info/git/guix/guix-wigust")
        (branch "master")
        (commit
          "ec542755f860e050f2355c0b305cafc386496abf"))
      (channel
        (name 'nonguix)
        (url "https://cgit.wugi.info/git/guix/nonguix")
        (branch "master")
        (commit
          "e899121adbaa13cfcaeae7a5c24921bffa645771")
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
