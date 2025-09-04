(define-module (wugi packages linux)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system linux-module)
  #:use-module (guix discovery)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (srfi srfi-26))

(define-syntax with-modules
  (lambda (x)
    (syntax-case x ()
      ((_ (modules ...) body ...)
       (with-syntax
           ((symbols
             (datum->syntax
              x
              (apply
               append
               (filter
                identity
                (map (lambda (module)
                       (and=> (false-if-exception (resolve-interface module))
                              (lambda (iface)
                                (map (lambda (symbol)
                                       `(,symbol ,`(@ ,module ,symbol)))
                                     (module-map (lambda (sym var) sym)
                                                 iface)))))
                     (syntax->datum #'(modules ...))))))))
         #'(let symbols
               body ...))))))

(define-syntax define-public
  (syntax-rules ()
    ((_ name value)
     (begin (module-define! (current-module) 'name value)
            (module-export! (current-module) '(name))))))

(when (and=> (and (false-if-exception (resolve-interface '(nongnu packages linux)))
                  (false-if-exception (resolve-interface '(gnu packages linux))))
             (lambda (module)
               (module-defined? module 'linux-libre-5.15)))
  (define-public linux-5.13-with-bpf
    (with-modules
     ((nongnu packages linux))

     (define linux-libre-deblob-scripts
       (@@ (gnu packages linux) linux-libre-deblob-scripts))

     (define make-linux-libre-source
       (@@ (gnu packages linux) make-linux-libre-source))

     (define %upstream-linux-source
       (@@ (gnu packages linux) %upstream-linux-source))

     (define source-with-patches
       (@@ (gnu packages linux) source-with-patches))

     (define %boot-logo-patch
       (@@ (gnu packages linux) %boot-logo-patch))

     (define %linux-libre-arm-export-__sync_icache_dcache-patch
       (@@ (gnu packages linux) %linux-libre-arm-export-__sync_icache_dcache-patch))

     (define make-linux-libre*
       (@@ (gnu packages linux) make-linux-libre*))

     (define* (kernel-config arch #:key variant)
       "Return a file-like object of the Linux-Libre build configuration file for
ARCH and optionally VARIANT, or #f if there is no such configuration."
       (define %auxiliary-files-path
         (make-parameter
          (map (cut string-append <> "/wugi/packages/aux-files")
               %load-path)))

       (define (search-auxiliary-file file-name)
         "Search the auxiliary FILE-NAME.  Return #f if not found."
         (search-path (%auxiliary-files-path) file-name))

       (let* ((name (string-append (if variant (string-append variant "-") "")
                                   (if (string=? "i386" arch) "i686" arch) ".conf"))
              (file (string-append "linux-libre/" name))
              (config (search-auxiliary-file file)))
         (and config (local-file config))))

     (define-public linux-libre-5.13-version "5.13.16")
     (define-public linux-libre-5.13-gnu-revision "gnu1")
     (define deblob-scripts-5.13
       (linux-libre-deblob-scripts
        linux-libre-5.13-version
        linux-libre-5.13-gnu-revision
        (base32 "0hj3w3vh1rj24xgl4v72mr6vaz1qzsnc5xzdfjga1zy84bw8lhkp")
        (base32 "1a0k9i8gnzkyvfr80f8xw2fnxfwddhz1pzicz9fh0y3jzzkzk45p")))
     (define-public linux-libre-5.13-pristine-source
       (let ((version linux-libre-5.13-version)
             (hash (base32 "1ljigvcg4q6ckr8kna3q5iyjsy7x5mrf1ycqfy0ibbhn9hbqjna9")))
         (make-linux-libre-source version
                                  (%upstream-linux-source version hash)
                                  deblob-scripts-5.13)))

     (define-public linux-libre-5.13-source
       (source-with-patches linux-libre-5.13-pristine-source
                            (list %boot-logo-patch
                                  %linux-libre-arm-export-__sync_icache_dcache-patch)))

     ;; See https://github.com/iovisor/bcc/blob/master/INSTALL.md#kernel-configuration
     (define %bpf-extra-linux-options
       `(;; Needed for probes
         ("CONFIG_UPROBE_EVENTS" . #t)
         ("CONFIG_KPROBE_EVENTS" . #t)
         ;; kheaders module also helpful for tracing
         ("CONFIG_IKHEADERS" . #t)
         ("CONFIG_BPF" . #t)
         ("CONFIG_BPF_SYSCALL" . #t)
         ("CONFIG_BPF_JIT_ALWAYS_ON" . #t)
         ;; optional, for tc filters
         ("CONFIG_NET_CLS_BPF" . m)
         ;; optional, for tc actions
         ("CONFIG_NET_ACT_BPF" . m)
         ("CONFIG_BPF_JIT" . #t)
         ;; for Linux kernel versions 4.1 through 4.6
         ;; ("CONFIG_HAVE_BPF_JIT" . y)
         ;; for Linux kernel versions 4.7 and later
         ("CONFIG_HAVE_EBPF_JIT" . #t)
         ;; optional, for kprobes
         ("CONFIG_BPF_EVENTS" . #t)
         ;; kheaders module
         ("CONFIG_IKHEADERS" . #t)))

     (define %my-extra-linux-options
       `(("CONFIG_DEBUG_INFO_BTF" . #t)
         ("CONFIG_FPROBE" . #t)))

     (define %default-extra-linux-options
       `(;; Make the kernel config available at /proc/config.gz
         ("CONFIG_IKCONFIG" . #t)
         ("CONFIG_IKCONFIG_PROC" . #t)
         ;; Some very mild hardening.
         ("CONFIG_SECURITY_DMESG_RESTRICT" . #t)
         ;; All kernels should have NAMESPACES options enabled
         ("CONFIG_NAMESPACES" . #t)
         ("CONFIG_UTS_NS" . #t)
         ("CONFIG_IPC_NS" . #t)
         ("CONFIG_USER_NS" . #t)
         ("CONFIG_PID_NS" . #t)
         ("CONFIG_NET_NS" . #t)
         ;; Various options needed for elogind service:
         ;; https://issues.guix.gnu.org/43078
         ("CONFIG_CGROUP_FREEZER" . #t)
         ("CONFIG_BLK_CGROUP" . #t)
         ("CONFIG_CGROUP_WRITEBACK" . #t)
         ("CONFIG_CGROUP_SCHED" . #t)
         ("CONFIG_CGROUP_PIDS" . #t)
         ("CONFIG_CGROUP_FREEZER" . #t)
         ("CONFIG_CGROUP_DEVICE" . #t)
         ("CONFIG_CGROUP_CPUACCT" . #t)
         ("CONFIG_CGROUP_PERF" . #t)
         ("CONFIG_SOCK_CGROUP_DATA" . #t)
         ("CONFIG_BLK_CGROUP_IOCOST" . #t)
         ("CONFIG_CGROUP_NET_PRIO" . #t)
         ("CONFIG_CGROUP_NET_CLASSID" . #t)
         ("CONFIG_MEMCG" . #t)
         ("CONFIG_MEMCG_SWAP" . #t)
         ("CONFIG_MEMCG_KMEM" . #t)
         ("CONFIG_CPUSETS" . #t)
         ("CONFIG_PROC_PID_CPUSET" . #t)
         ;; Allow disk encryption by default
         ("CONFIG_DM_CRYPT" . m)
         ;; Support zram on all kernel configs
         ("CONFIG_ZSWAP" . #t)
         ("CONFIG_ZSMALLOC" . #t)
         ("CONFIG_ZRAM" . m)
         ;; Accessibility support.
         ("CONFIG_ACCESSIBILITY" . #t)
         ("CONFIG_A11Y_BRAILLE_CONSOLE" . #t)
         ("CONFIG_SPEAKUP" . m)
         ("CONFIG_SPEAKUP_SYNTH_SOFT" . m)
         ;; Modules required for initrd:
         ("CONFIG_NET_9P" . m)
         ("CONFIG_NET_9P_VIRTIO" . m)
         ("CONFIG_VIRTIO_BLK" . m)
         ("CONFIG_VIRTIO_NET" . m)
         ("CONFIG_VIRTIO_PCI" . m)
         ("CONFIG_VIRTIO_BALLOON" . m)
         ("CONFIG_VIRTIO_MMIO" . m)
         ("CONFIG_FUSE_FS" . m)
         ("CONFIG_CIFS" . m)
         ("CONFIG_9P_FS" . m)))

     (define-public linux-libre-5.13-with-bpf
       (let ((base-linux-libre
              (make-linux-libre*
               linux-libre-5.13-version
               linux-libre-5.13-gnu-revision
               linux-libre-5.13-source
               '("x86_64-linux" "i686-linux" "armhf-linux"
                 "aarch64-linux" "powerpc64le-linux" "riscv64-linux")
               #:extra-version "bpf"
               #:configuration-file kernel-config
               #:extra-options
               (append %bpf-extra-linux-options
                       %my-extra-linux-options
                       %default-extra-linux-options))))
         (package
           (inherit base-linux-libre)
           (inputs (modify-inputs (package-inputs base-linux-libre)
                     (prepend cpio)))
           (synopsis "Linux-libre with BPF support")
           (description "This package provides GNU Linux-Libre with support
for @acronym{BPF, the Berkeley Packet Filter}."))))

     (define (linux-urls version)
       "Return a list of URLS for Linux VERSION."
       (list (string-append "https://www.kernel.org/pub/linux/kernel/v"
                            (version-major version) ".x/linux-" version ".tar.xz")))

     (define* (corrupt-linux freedo version hash #:key (name "linux"))
       (package
         (inherit freedo)
         (name name)
         (version version)
         (source (origin
                   (method url-fetch)
                   (uri (linux-urls version))
                   (sha256 (base32 hash))))
         (home-page "https://www.kernel.org/")
         (synopsis "Linux kernel with nonfree binary blobs included")
         (description
          "The unmodified Linux kernel, including nonfree blobs, for running Guix
System on hardware which requires nonfree software to function.")))

     (corrupt-linux linux-libre-5.13-with-bpf "5.13.16"
                    "1ljigvcg4q6ckr8kna3q5iyjsy7x5mrf1ycqfy0ibbhn9hbqjna9"))))

(when (and=> (and (false-if-exception (resolve-interface '(nongnu packages linux)))
                  (false-if-exception (resolve-interface '(gnu packages linux))))
             (lambda (module)
               (module-defined? module 'linux-libre-5.15)))
  (define-public linux-5.15-with-bpf
    (with-modules
     ((nongnu packages linux))
     (corrupt-linux linux-libre-5.15))))

(when (and=> (false-if-exception (resolve-interface '(nonguix licenses)))
             (lambda (module)
               (module-defined? module 'nonfree)))
  (define-public linux-firmware
    (with-modules
     ((nonguix licenses))
     (package
       (name "linux-firmware")
       (version "20210818")
       (source
        (origin
          (method url-fetch)
          (uri
           (string-append
            "https://git.kernel.org/pub/scm/linux/kernel"
            "/git/firmware/linux-firmware.git/snapshot/"
            "linux-firmware-" version ".tar.gz"))
          (sha256
           (base32
            "0842k00kjh89497vvd7zy3j8d5xq180q2zkqmq0yivp2xkzvbwfc"))))
       (build-system gnu-build-system)
       (arguments
        `(#:tests? #f
          #:make-flags (list (string-append "DESTDIR=" (assoc-ref %outputs "out")))
          #:phases
          (modify-phases %standard-phases
            (delete 'validate-runpath))))
       (home-page
        "https://git.kernel.org/pub/scm/linux/kernel/git/firmware/linux-firmware.git")
       (synopsis "Nonfree firmware blobs for Linux")
       (description "Nonfree firmware blobs for enabling
support for various hardware in the Linux kernel.  This is a large package
which may be overkill if your hardware is supported by one of the smaller
firmware packages.")
       (license
        (nonfree
         (string-append
          "https://git.kernel.org/pub/scm/linux/kernel/git/"
          "firmware/linux-firmware.git/plain/WHENCE")))))))
