(define-module (wugi packages rust-crates)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module ((gnu packages rust-sources) #:prefix package:)
  #:export (lookup-cargo-inputs))

(define rust-aho-corasick-1.1.4
  (crate-source "aho-corasick" "1.1.4"
                "00a32wb2h07im3skkikc495jvncf62jl6s96vwc7bhi70h9imlyx"))

(define rust-anstream-0.6.21
  (crate-source "anstream" "0.6.21"
                "0jjgixms4qjj58dzr846h2s29p8w7ynwr9b9x6246m1pwy0v5ma3"))

(define rust-anstyle-1.0.13
  (crate-source "anstyle" "1.0.13"
                "0y2ynjqajpny6q0amvfzzgw0gfw3l47z85km4gvx87vg02lcr4ji"))

(define rust-anstyle-parse-0.2.7
  (crate-source "anstyle-parse" "0.2.7"
                "1hhmkkfr95d462b3zf6yl2vfzdqfy5726ya572wwg8ha9y148xjf"))

(define rust-anstyle-query-1.1.5
  (crate-source "anstyle-query" "1.1.5"
                "1p6shfpnbghs6jsa0vnqd8bb8gd7pjd0jr7w0j8jikakzmr8zi20"))

(define rust-anstyle-wincon-3.0.11
  (crate-source "anstyle-wincon" "3.0.11"
                "0zblannm70sk3xny337mz7c6d8q8i24vhbqi42ld8v7q1wjnl7i9"))

(define rust-anyhow-1.0.100
  (crate-source "anyhow" "1.0.100"
                "0qbfmw4hhv2ampza1csyvf1jqjs2dgrj29cv3h3sh623c6qvcgm2"))

(define rust-autocfg-1.5.0
  (crate-source "autocfg" "1.5.0"
                "1s77f98id9l4af4alklmzq46f21c980v13z2r1pcxx6bqgw0d1n0"))

(define rust-bitflags-1.3.2
  (crate-source "bitflags" "1.3.2"
                "12ki6w8gn1ldq7yz9y680llwk5gmrhrzszaa17g1sbrw2r2qvwxy"))

(define rust-bitflags-2.10.0
  (crate-source "bitflags" "2.10.0"
                "1lqxwc3625lcjrjm5vygban9v8a6dlxisp1aqylibiaw52si4bl1"))

(define rust-bitvec-1.0.1
  (crate-source "bitvec" "1.0.1"
                "173ydyj2q5vwj88k6xgjnfsshs4x9wbvjjv7sm0h36r34hn87hhv"))

(define rust-cc-1.2.54
  (crate-source "cc" "1.2.54"
                "10q5q3kzcqg9s1zrgpk9sn1a98xjnz177ixkkkx9qbfnpwdwhm33"))

(define rust-cfg-if-1.0.4
  (crate-source "cfg-if" "1.0.4"
                "008q28ajc546z5p2hcwdnckmg0hia7rnx52fni04bwqkzyrghc4k"))

(define rust-colorchoice-1.0.4
  (crate-source "colorchoice" "1.0.4"
                "0x8ymkz1xr77rcj1cfanhf416pc4v681gmkc9dzb3jqja7f62nxh"))

(define rust-env-filter-0.1.4
  (crate-source "env_filter" "0.1.4"
                "1qk8yn4lsqzxsz025kf4kaabika6aidykqih3c2p1jjms9cw5wqv"))

(define rust-env-logger-0.11.8
  (crate-source "env_logger" "0.11.8"
                "17q6zbjam4wq75fa3m4gvvmv3rj3ch25abwbm84b28a0j3q67j0k"))

(define rust-errno-0.3.14
  (crate-source "errno" "0.3.14"
                "1szgccmh8vgryqyadg8xd58mnwwicf39zmin3bsn63df2wbbgjir"))

(define rust-evdev-0.12.2
  (crate-source "evdev" "0.12.2"
                "19qh6r1z4v8ja6qqigjbg9vckbhlycc6wkqgzfz9fcln7almaq5b"))

(define rust-find-msvc-tools-0.1.8
  (crate-source "find-msvc-tools" "0.1.8"
                "1nv8hn78xphg04l6w7iq1v8lsmmqx6ripbig18qn92m9r2yb14c5"))

(define rust-funty-2.0.0
  (crate-source "funty" "2.0.0"
                "177w048bm0046qlzvp33ag3ghqkqw4ncpzcm5lq36gxf2lla7mg6"))

(define rust-hex-0.4.3
  (crate-source "hex" "0.4.3"
                "0w1a4davm1lgzpamwnba907aysmlrnygbqmfis2mqjx5m552a93z"))

(define rust-is-terminal-polyfill-1.70.2
  (crate-source "is_terminal_polyfill" "1.70.2"
                "15anlc47sbz0jfs9q8fhwf0h3vs2w4imc030shdnq54sny5i7jx6"))

(define rust-jiff-0.2.18
  (crate-source "jiff" "0.2.18"
                "0l6g3vkqa7imd3nsvizmb648jn0gcadiydh3zq4rjvcxqjj8szp6"))

(define rust-jiff-static-0.2.18
  (crate-source "jiff-static" "0.2.18"
                "0y3fks93ij3frb1jnpzi68b9kssm3rvwpmkgdjlakv4py7klxj70"))

(define rust-libc-0.2.180
  (crate-source "libc" "0.2.180"
                "1z2n7hl10fnk1xnv19ahhqxwnb4qi9aclnl6gigim2aaahw5mhxw"))

(define rust-linux-raw-sys-0.4.15
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "linux-raw-sys" "0.4.15"
                "1aq7r2g7786hyxhv40spzf2nhag5xbw2axxc1k8z5k1dsgdm4v6j"))

(define rust-log-0.4.29
  (crate-source "log" "0.4.29"
                "15q8j9c8g5zpkcw0hnd6cf2z7fxqnvsjh3rw5mv5q10r83i34l2y"))

(define rust-memchr-2.7.6
  (crate-source "memchr" "2.7.6"
                "0wy29kf6pb4fbhfksjbs05jy2f32r2f3r1ga6qkmpz31k79h0azm"))

(define rust-memoffset-0.6.5
  (crate-source "memoffset" "0.6.5"
                "1kkrzll58a3ayn5zdyy9i1f1v3mx0xgl29x0chq614zazba638ss"))

(define rust-nix-0.23.2
  (crate-source "nix" "0.23.2"
                "0p5kxhm5d8lry0szqbsllpcb5i3z7lg1dkglw0ni2l011b090dwg"))

(define rust-once-cell-polyfill-1.70.2
  (crate-source "once_cell_polyfill" "1.70.2"
                "1zmla628f0sk3fhjdjqzgxhalr2xrfna958s632z65bjsfv8ljrq"))

(define rust-portable-atomic-1.13.0
  (crate-source "portable-atomic" "1.13.0"
                "0l79rf3pzlxmmrylr1c4k61qn8hzs6hzz69yk738pdcvsvj7d5zq"))

(define rust-portable-atomic-util-0.2.4
  (crate-source "portable-atomic-util" "0.2.4"
                "01rmx1li07ixsx3sqg2bxqrkzk7b5n8pibwwf2589ms0s3cg18nq"))

(define rust-proc-macro2-1.0.106
  (crate-source "proc-macro2" "1.0.106"
                "0d09nczyaj67x4ihqr5p7gxbkz38gxhk4asc0k8q23g9n85hzl4g"))

(define rust-procfs-0.17.0
  (crate-source "procfs" "0.17.0"
                "17swyjqinpb745f07dpdi7c8q37hxvhx9xmmsi2dhxaj2kc74nyc"))

(define rust-procfs-core-0.17.0
  (crate-source "procfs-core" "0.17.0"
                "1v0jdbyc1rq1x22m0wn7n4iq4h86gdls38wqfg06zc29hcnz1793"))

(define rust-quote-1.0.44
  (crate-source "quote" "1.0.44"
                "1r7c7hxl66vz3q9qizgjhy77pdrrypqgk4ghc7260xvvfb7ypci1"))

(define rust-radium-0.7.0
  (crate-source "radium" "0.7.0"
                "02cxfi3ky3c4yhyqx9axqwhyaca804ws46nn4gc1imbk94nzycyw"))

(define rust-regex-1.12.2
  (crate-source "regex" "1.12.2"
                "1m14zkg6xmkb0q5ah3y39cmggclsjdr1wpxfa4kf5wvm3wcw0fw4"))

(define rust-regex-automata-0.4.13
  (crate-source "regex-automata" "0.4.13"
                "070z0j23pjfidqz0z89id1fca4p572wxpcr20a0qsv68bbrclxjj"))

(define rust-regex-syntax-0.8.8
  (crate-source "regex-syntax" "0.8.8"
                "0n7ggnpk0r32rzgnycy5xrc1yp2kq19m6pz98ch3c6dkaxw9hbbs"))

(define rust-rustix-0.38.44
  (crate-source "rustix" "0.38.44"
                "0m61v0h15lf5rrnbjhcb9306bgqrhskrqv7i1n0939dsw8dbrdgx"))

(define rust-serde-core-1.0.228
  (crate-source "serde_core" "1.0.228"
                "1bb7id2xwx8izq50098s5j2sqrrvk31jbbrjqygyan6ask3qbls1"))

(define rust-serde-derive-1.0.228
  (crate-source "serde_derive" "1.0.228"
                "0y8xm7fvmr2kjcd029g9fijpndh8csv5m20g4bd76w8qschg4h6m"))

(define rust-shlex-1.3.0
  (crate-source "shlex" "1.3.0"
                "0r1y6bv26c1scpxvhg2cabimrmwgbp4p3wy6syj9n0c4s3q2znhg"))

(define rust-signal-hook-0.3.18
  (crate-source "signal-hook" "0.3.18"
                "1qnnbq4g2vixfmlv28i1whkr0hikrf1bsc4xjy2aasj2yina30fq"))

(define rust-signal-hook-registry-1.4.8
  (crate-source "signal-hook-registry" "1.4.8"
                "06vc7pmnki6lmxar3z31gkyg9cw7py5x9g7px70gy2hil75nkny4"))

(define rust-syn-2.0.114
  (crate-source "syn" "2.0.114"
                "0akw62dizhyrkf3ym1jsys0gy1nphzgv0y8qkgpi6c1s4vghglfl"))

(define rust-tap-1.0.1
  (crate-source "tap" "1.0.1"
                "0sc3gl4nldqpvyhqi3bbd0l9k7fngrcl4zs47n314nqqk4bpx4sm"))

(define rust-thiserror-1.0.69
  (crate-source "thiserror" "1.0.69"
                "0lizjay08agcr5hs9yfzzj6axs53a2rgx070a1dsi3jpkcrzbamn"))

(define rust-thiserror-impl-1.0.69
  (crate-source "thiserror-impl" "1.0.69"
                "1h84fmn2nai41cxbhk6pqf46bxqq1b344v8yz089w1chzi76rvjg"))

(define rust-unicode-ident-1.0.22
  (crate-source "unicode-ident" "1.0.22"
                "1x8xrz17vqi6qmkkcqr8cyf0an76ig7390j9cnqnk47zyv2gf4lk"))

(define rust-utf8parse-0.2.2
  (crate-source "utf8parse" "0.2.2"
                "088807qwjq46azicqwbhlmzwrbkz7l4hpw43sdkdyyk524vdxaq6"))

(define rust-windows-aarch64-gnullvm-0.52.6
  (crate-source "windows_aarch64_gnullvm" "0.52.6"
                "1lrcq38cr2arvmz19v32qaggvj8bh1640mdm9c2fr877h0hn591j"))

(define rust-windows-aarch64-msvc-0.52.6
  (crate-source "windows_aarch64_msvc" "0.52.6"
                "0sfl0nysnz32yyfh773hpi49b1q700ah6y7sacmjbqjjn5xjmv09"))

(define rust-windows-i686-gnu-0.52.6
  (crate-source "windows_i686_gnu" "0.52.6"
                "02zspglbykh1jh9pi7gn8g1f97jh1rrccni9ivmrfbl0mgamm6wf"))

(define rust-windows-i686-gnullvm-0.52.6
  (crate-source "windows_i686_gnullvm" "0.52.6"
                "0rpdx1537mw6slcpqa0rm3qixmsb79nbhqy5fsm3q2q9ik9m5vhf"))

(define rust-windows-i686-msvc-0.52.6
  (crate-source "windows_i686_msvc" "0.52.6"
                "0rkcqmp4zzmfvrrrx01260q3xkpzi6fzi2x2pgdcdry50ny4h294"))

(define rust-windows-link-0.2.1
  (crate-source "windows-link" "0.2.1"
                "1rag186yfr3xx7piv5rg8b6im2dwcf8zldiflvb22xbzwli5507h"))

(define rust-windows-sys-0.59.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "windows-sys" "0.59.0"
                "0fw5672ziw8b3zpmnbp9pdv1famk74f1l9fcbc3zsrzdg56vqf0y"))

(define rust-windows-sys-0.61.2
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "windows-sys" "0.61.2"
                "1z7k3y9b6b5h52kid57lvmvm05362zv1v8w0gc7xyv5xphlp44xf"))

(define rust-windows-targets-0.52.6
  (crate-source "windows-targets" "0.52.6"
                "0wwrx625nwlfp7k93r2rra568gad1mwd888h1jwnl0vfg5r4ywlv"))

(define rust-windows-x86-64-gnu-0.52.6
  (crate-source "windows_x86_64_gnu" "0.52.6"
                "0y0sifqcb56a56mvn7xjgs8g43p33mfqkd8wj1yhrgxzma05qyhl"))

(define rust-windows-x86-64-gnullvm-0.52.6
  (crate-source "windows_x86_64_gnullvm" "0.52.6"
                "03gda7zjx1qh8k9nnlgb7m3w3s1xkysg55hkd1wjch8pqhyv5m94"))

(define rust-windows-x86-64-msvc-0.52.6
  (crate-source "windows_x86_64_msvc" "0.52.6"
                "1v7rb5cibyzx8vak29pdrk8nx9hycsjs4w0jgms08qk49jl6v7sq"))

(define rust-wyz-0.5.1
  (crate-source "wyz" "0.5.1"
                "1vdrfy7i2bznnzjdl9vvrzljvs4s3qm8bnlgqwln6a941gy61wq5"))

(define-cargo-inputs lookup-cargo-inputs
                     (poe-scroll-click =>
                                       (list rust-aho-corasick-1.1.4
                                        rust-anstream-0.6.21
                                        rust-anstyle-1.0.13
                                        rust-anstyle-parse-0.2.7
                                        rust-anstyle-query-1.1.5
                                        rust-anstyle-wincon-3.0.11
                                        rust-anyhow-1.0.100
                                        rust-autocfg-1.5.0
                                        rust-bitflags-1.3.2
                                        rust-bitflags-2.10.0
                                        rust-bitvec-1.0.1
                                        rust-cc-1.2.54
                                        rust-cfg-if-1.0.4
                                        rust-colorchoice-1.0.4
                                        rust-env-filter-0.1.4
                                        rust-env-logger-0.11.8
                                        rust-errno-0.3.14
                                        rust-evdev-0.12.2
                                        rust-find-msvc-tools-0.1.8
                                        rust-funty-2.0.0
                                        rust-hex-0.4.3
                                        rust-is-terminal-polyfill-1.70.2
                                        rust-jiff-0.2.18
                                        rust-jiff-static-0.2.18
                                        rust-libc-0.2.180
                                        rust-linux-raw-sys-0.4.15
                                        rust-log-0.4.29
                                        rust-memchr-2.7.6
                                        rust-memoffset-0.6.5
                                        rust-nix-0.23.2
                                        rust-once-cell-polyfill-1.70.2
                                        rust-portable-atomic-1.13.0
                                        rust-portable-atomic-util-0.2.4
                                        rust-proc-macro2-1.0.106
                                        rust-procfs-0.17.0
                                        rust-procfs-core-0.17.0
                                        rust-quote-1.0.44
                                        rust-radium-0.7.0
                                        rust-regex-1.12.2
                                        rust-regex-automata-0.4.13
                                        rust-regex-syntax-0.8.8
                                        rust-rustix-0.38.44
                                        rust-serde-core-1.0.228
                                        rust-serde-derive-1.0.228
                                        rust-shlex-1.3.0
                                        rust-signal-hook-0.3.18
                                        rust-signal-hook-registry-1.4.8
                                        rust-syn-2.0.114
                                        rust-tap-1.0.1
                                        rust-thiserror-1.0.69
                                        rust-thiserror-impl-1.0.69
                                        rust-unicode-ident-1.0.22
                                        rust-utf8parse-0.2.2
                                        rust-windows-link-0.2.1
                                        rust-windows-sys-0.59.0
                                        rust-windows-sys-0.61.2
                                        rust-windows-targets-0.52.6
                                        rust-windows-aarch64-gnullvm-0.52.6
                                        rust-windows-aarch64-msvc-0.52.6
                                        rust-windows-i686-gnu-0.52.6
                                        rust-windows-i686-gnullvm-0.52.6
                                        rust-windows-i686-msvc-0.52.6
                                        rust-windows-x86-64-gnu-0.52.6
                                        rust-windows-x86-64-gnullvm-0.52.6
                                        rust-windows-x86-64-msvc-0.52.6
                                        rust-wyz-0.5.1)))

