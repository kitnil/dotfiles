(define-module (packages wm)
  #:use-module (gnu packages stb)
  #:use-module (gnu packages benchmark)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages wm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system haskell)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpd)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages music)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages time)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public libliftoff
  (package
    (name "libliftoff")
    (version "0.4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/emersion/libliftoff")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256 (base32 "1ikjp638d655ycaqkdnzhb12d29kkbb3a46lqhbhsfc8vsqj3z1l"))))
    (build-system meson-build-system)
    (inputs (list libdrm))
    (native-inputs (list pkg-config))
    (home-page "https://gitlab.freedesktop.org/emersion/libliftoff")
    (synopsis "Lightweight KMS plane library")
    (description "libliftoff eases the use of KMS planes from userspace
without standing in your way. Users create \"virtual planes\" called layers,
set KMS properties on them, and libliftoff will pick hardware planes for these
layers if possible.")
    (license license:expat)))

(define-public vkroots
  (let ((commit "d5ef31abc7cb5c69aee4bcb67b10dd543c1ff7ac")
        (revision "0"))
    (package
      (name "vkroots")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Joshua-Ashton/vkroots")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "0g2mh8l0xzxzr4yjyafzv76n7jk9043dcbf5mpqwpwmjx88m5nc0"))))
      (build-system meson-build-system)
      (arguments (list #:phases #~(modify-phases %standard-phases
                                    (add-after 'unpack 'patch-vulkan
                                      (lambda _
                                        (substitute* "gen/make_vkroots"
                                          (("\\.\\.") (getcwd)))))
                                    (add-before 'install 'gen-vkroots
                                      (lambda _
                                        (invoke "python3"
                                                "../source/gen/make_vkroots"
                                                "-x"
                                                (string-append
                                                 #$(this-package-native-input "vulkan-headers")
                                                 "/share/vulkan/registry/vk.xml")))))))
      (native-inputs (list python vulkan-headers))
      (home-page "https://github.com/Joshua-Ashton/vkroots")
      (synopsis "Simple method of making Vulkan layers")
      (description "vkroots is a framework for writing Vulkan layers that
takes all the complexity away from you.")
      (license license:expat))))

(define-public libavif-1.0
  (package
    (inherit libavif)
    (version "1.0.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/AOMediaCodec/libavif")
                    (commit (string-append "v" version))))
              (file-name (git-file-name (package-name libavif) version))
              (sha256
               (base32 "0k72q7yvfdn92wkslyifw14319nm981a8r3kd84i4ylxmrkgi0zm"))))))

(define %gamescope-version "3.14.2")

(define reshade-for-gamescope
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/Joshua-Ashton/reshade")
          (commit "9fdbea6892f9959fdc18095d035976c574b268b7")))
    (file-name (git-file-name "reshade-for-gamescope" %gamescope-version))
    (sha256
     (base32 "0wvrmjsszlsfy6p47i775x784xnsc130zvj18zjgx6fwq6amcf4i"))))

(define-public gamescope
  (package
    (name "gamescope")
    (version %gamescope-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ValveSoftware/gamescope")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0x7gh1rr2ismqfkaa4wm7025acjpgmims41iwzdcps5pg8nxmmhh"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags #~(list "-Dpipewire=enabled"
                                     "-Denable_openvr_support=false"
                                     "-Dforce_fallback_for=[]"
                                     (string-append "-Dc_args=-DHWDATA_PNP_IDS=\""
                                                    #$(this-package-native-input "hwdata")
                                                    "/share/hwdata/pnp.ids\""))
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'patch-deps
                          (lambda _
                            (substitute* "src/reshade_effect_manager.cpp"
                              (("/usr") #$output))
                            (substitute* "src/meson.build"
                              ;; patch stb
                              (("dependency\\('stb'\\)")
                               (format #f "declare_dependency(include_directories: ['~a'])"
                                       (string-join
                                        '#$(map (lambda (label) (this-package-native-input label))
                                                (list "stb-image"
                                                      "stb-image-resize"
                                                      "stb-image-write"))
                                        "','")))
                              ;; patch libdisplay-info
                              (("< 0.2.0")
                               (string-append "<= " #$(package-version (this-package-input "libdisplay-info"))))
                              (("reshade/") (string-append #$reshade-for-gamescope "/"))
                              (("../thirdparty/SPIRV-Headers") #$(this-package-native-input "spirv-headers"))))))))
    (inputs
     (list glm
           libavif-1.0
           libcap
           libdisplay-info
           libdrm
           libinput
           libliftoff
           libx11
           libxcomposite
           libxcursor
           libxdamage
           libxext
           libxkbcommon
           libxmu
           libxrender
           libxres
           libxt
           libxtst
           pipewire
           sdl2
           vulkan-loader
           wayland
           wlroots))
    (native-inputs
     (list benchmark
           ;; Lazily resolve the gcc-toolchain-12 to avoid a circular dependency.
           (module-ref (resolve-interface '(gnu packages commencement))
                       'gcc-toolchain-12)
           glslang
           `(,hwdata "pnp")
           pkg-config
           stb-image
           stb-image-resize
           stb-image-write
           spirv-headers
           vkroots
           vulkan-headers
           wayland-protocols))
    (home-page "https://github.com/ValveSoftware/gamescope") 
    (synopsis "Micro-compositor for running games")
    (description
     "gamescope is a micro-compositor for running games.  Its goal is to
provide an isolated compositor that is tailored towards gaming and supports
many gaming-centric features such as:
@itemize
@item Spoofing resolutions.
@item Upscaling.
@item Limiting framerates.
@end itemize")
    (license license:bsd-2)))
