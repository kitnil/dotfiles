(define-module (wugi manifests desktop)
  #:use-module (guix profiles)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages apl)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:export (%desktop-manifest))

(define (%desktop-manifest)
  (define menus
    (list dmenu wofi))

  (define extra-fonts
    (list font-abattis-cantarell
          font-adobe-source-code-pro
          font-adobe-source-han-sans
          font-adobe-source-han-serif
          font-adobe-source-sans
          font-adobe-source-sans-pro
          font-adobe-source-serif
          font-adobe-source-serif-pro
          font-adobe100dpi
          font-adobe75dpi
          font-alias
          ;; font-amiri
          font-anonymous-pro
          font-anonymous-pro-minus
          font-apl2741-unicode
          font-aporetic
          font-arabic-misc
          font-arapey
          font-arphic-ukai
          font-artifika
          font-atkinson-hyperlegible
          font-atui-feather
          font-bitstream-vera
          font-blackfoundry-inria
          font-borg-sans-mono
          font-bravura
          font-canada1500
          font-cardo
          font-carlito
          font-catamaran
          font-charter
          font-chiron-hei-hk
          font-chiron-sung-hk
          font-chivo
          font-cica
          font-cns11643-swjz
          font-comic-neue
          font-cormorant
          ;; font-cozette
          font-cronyx-cyrillic
          font-culmus
          font-dec-misc
          font-dina
          ;; font-dongle
          font-dosis
          font-dseg
          font-et-book
          font-fantasque-sans
          font-fira-code
          font-fira-go
          font-fira-mono
          font-fira-sans
          font-fontna-yasashisa-antique
          font-gfs-ambrosia
          font-ghostscript
          font-gnu-freefont
          font-gnu-unifont
          font-go
          font-google-material-design-icons
          font-google-noto
          font-google-noto-emoji
          font-google-noto-sans-cjk
          font-google-noto-serif-cjk
          font-google-roboto
          font-hachimarupop
          font-hack
          font-hermit
          font-ibm-plex
          font-inconsolata
          font-intel-one-mono
          font-iosevka
          font-iosevka-aile
          font-iosevka-comfy
          font-iosevka-curly
          font-iosevka-curly-slab
          font-iosevka-etoile
          font-iosevka-slab
          font-iosevka-ss01
          font-iosevka-ss02
          font-iosevka-ss03
          font-iosevka-ss04
          font-iosevka-ss05
          font-iosevka-ss06
          font-iosevka-ss07
          font-iosevka-ss08
          font-iosevka-ss09
          font-iosevka-ss10
          font-iosevka-ss11
          font-iosevka-ss12
          font-iosevka-ss13
          font-iosevka-ss14
          font-iosevka-ss15
          font-iosevka-ss16
          font-iosevka-ss17
          font-iosevka-ss18
          font-iosevka-term
          font-iosevka-term-slab
          font-ipa
          font-ipa-ex
          font-ipa-mj-mincho
          font-isas-misc
          font-jetbrains-mono
          font-jigmo
          font-juliamono
          font-junicode
          font-kochi-substitute
          font-koruri
          font-latin-modern
          font-lato
          font-liberation
          font-libertinus
          font-libre-franklin
          font-lilex
          font-linuxlibertine
          font-lisnoti
          font-lohit
          font-lxgw-heartserif
          font-lxgw-neozhisong
          font-lxgw-wenkai
          font-lxgw-wenkai-tc
          font-mathjax
          font-meera-inimai
          font-micro-misc
          font-microsoft-cascadia
          font-misc-cyrillic
          font-misc-ethiopic
          font-misc-misc
          font-monaspace
          font-mononoki
          font-montserrat
          font-mplus-testflight
          font-mutt-misc
          font-opendyslexic
          font-openmoji
          font-orbitron
          font-oswald
          font-overpass
          font-paytone-one
          font-plangothic
          font-plemoljp
          font-public-sans
          font-rachana
          font-recursive
          font-sarasa-gothic
          font-sazanami
          font-schumacher-misc
          font-scientifica
          font-screen-cyrillic
          font-sil-andika
          font-sil-charis
          font-sil-ezra
          font-sil-gentium
          font-sony-misc
          font-space-grotesk
          font-spleen
          font-stix-two
          font-sun-misc
          font-takao
          font-tamzen
          font-teko
          font-terminus
          font-termsyn
          font-tex-gyre
          font-tuffy
          font-un
          font-unscii
          font-util
          font-vazir
          font-velvetyne-jgs
          font-victor-mono
          font-winitzki-cyrillic
          font-xfree86-type1))

  (define fonts
    (append (list font-adwaita
                  font-awesome
                  font-dejavu
                  font-wqy-microhei
                  font-wqy-zenhei
                  fontconfig)
            extra-fonts))

  (packages->manifest (append (list flatpak)
                              fonts
                              menus)))

(%desktop-manifest)
