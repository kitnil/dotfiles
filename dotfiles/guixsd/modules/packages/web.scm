;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (packages web)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages web)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:))

(define-public homer
  (package
    (name "homer")
    (version "20.07.2")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append
             "https://github.com/bastienwirtz/homer/releases/download/v"
             version "/homer.zip"))
       (file-name (string-append name "-" version ".zip"))
       (sha256
        (base32
         "1m2g8v0q9mj0w8ndijig95chalqwnskyfc7kiyb8l8kid137bs5v"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (data (string-append out "/share/homer")))
           (mkdir-p data)
           (copy-recursively (assoc-ref %build-inputs "source") data)
           (symlink "/etc/homer/config.yml"
                    (string-append data "/assets/config.yml"))
           #t))))
    (home-page "https://github.com/bastienwirtz/homer/")
    (synopsis "Static homepage for server to keep your services on hand")
    (description "Homer is a full static html/js dashboard, generated from the
source in /src using webpack.  It's meant to be served by an HTTP server.")
    (license license:expat)))

(define lua-nginx-module
  (let ((version "0.10.15"))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/openresty/lua-nginx-module")
            (commit (string-append "v" version))))
      (file-name (git-file-name "lua-nginx-module" version))
      (sha256
       (base32
        "1j216isp0546hycklbr5wi8mlga5hq170hk7f2sm16sfavlkh5gz")))))

(define-public nginx-lua
  (package
    (inherit nginx)
    (name "nginx-lua")
    (inputs
     `(("luajit" ,luajit)
       ("lua-nginx-module" ,lua-nginx-module)
       ,@(package-inputs nginx)))
    (arguments
     (substitute-keyword-arguments 
         `(#:configure-flags
           (list (string-append "--add-module="
                                (assoc-ref %build-inputs "lua-nginx-module")))
           ,@(package-arguments nginx))
       ((#:phases phases)
        `(modify-phases ,phases
           (add-before 'configure 'set-luajit-env
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((luajit (assoc-ref inputs "luajit")))
                 (setenv "LUAJIT_LIB"
                         (string-append luajit "/lib"))
                 (setenv "LUAJIT_INC" 
                         (string-append luajit "/include/luajit-2.1"))
                 #t)))))))
    (synopsis "NGINX with Lua module")
    (description "This package provides NGINX builded with Lua support.")))

(define-public lua-resty-core
  (package
    (name "lua-resty-core")
    (version "0.1.17")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/openresty/lua-resty-core")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "11fyli6yrg7b91nv9v2sbrc6y7z3h9lgf4lrrhcjk2bb906576a0"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((luajit-major+minor ,(version-major+minor (package-version lua)))
                (package-lua-resty (lambda (input output)
                                     (mkdir-p (string-append output "/lib/lua"))
                                     (copy-recursively (string-append input "/lib/resty")
                                                       (string-append output "/lib/lua/resty"))
                                     (copy-recursively (string-append input "/lib/ngx")
                                                       (string-append output "/lib/ngx"))
                                     (symlink (string-append output "/lib/lua/resty")
                                              (string-append output "/lib/resty")))))
           (package-lua-resty (assoc-ref %build-inputs "source")
                              (assoc-ref %outputs "out")))
         #t)))
    (home-page "https://github.com/openresty/lua-resty-core")
    (synopsis "Lua API for NGINX")
    (description "This package provides a FFI-based Lua API for
@code{ngx_http_lua_module} or @code{ngx_stream_lua_module}.")
    (license license:bsd-2)))

(define-public lua-resty-lrucache
  (package
    (name "lua-resty-lrucache")
    (version "0.09")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/openresty/lua-resty-lrucache")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1mwiy55qs8bija1kpgizmqgk15ijizzv4sa1giaz9qlqs2kqd7q2"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((luajit-major+minor ,(version-major+minor (package-version lua)))
                (package-lua-resty (lambda (input output)
                                     (mkdir-p (string-append output "/lib/lua/" luajit-major+minor))
                                     (copy-recursively (string-append input "/lib/resty")
                                                       (string-append output "/lib/lua/" luajit-major+minor  "/resty"))
                                     (symlink (string-append output "/lib/lua/" luajit-major+minor "/resty")
                                              (string-append output "/lib/resty")))))
           (package-lua-resty (assoc-ref %build-inputs "source")
                              (assoc-ref %outputs "out")))
         #t)))
    (home-page "https://github.com/openresty/lua-resty-lrucache")
    (synopsis "Lua LRU cache based on the LuaJIT FFI")
    (description
     "This package provides Lua LRU cache based on the LuaJIT FFI.")
    (license license:bsd-2)))

(define-public lua-resty-signal
  (package
    (name "lua-resty-signal")
    (version "0.02")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/openresty/lua-resty-signal")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "13y1pqn45y49mhqwywasfdsid46d0c33yi6mrnracbnmvyxz1cif"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;TODO: Run the test suite.

       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'install 'install-lua
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (use-modules (guix build utils))
             (let* ((luajit-major+minor ,(version-major+minor (package-version lua)))
                    (package-lua-resty (lambda (input output)
                                         (mkdir-p (string-append output "/lib/lua/" luajit-major+minor))
                                         (copy-recursively (string-append input "/lib/resty")
                                                           (string-append output "/lib/lua/" luajit-major+minor  "/resty"))
                                         (symlink (string-append output "/lib/lua/" luajit-major+minor "/resty")
                                                  (string-append output "/lib/resty")))))
               (package-lua-resty (assoc-ref inputs "source")
                                  (assoc-ref outputs "out")))
             #t)))))
    (home-page "https://github.com/openresty/lua-resty-signal")
    (synopsis "Lua library for killing or sending signals to Linux processes")
    (description "This package provides Lua library for killing or sending
signals to Linux processes.")
    (license license:bsd-3)))

(define-public lua-resty-shell
  (package
    (name "lua-resty-shell")
    (version "0.03")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/openresty/lua-resty-shell")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1s6g04ip4hr97r2pd8ry3alq063604s9a3l0hn9nsidh81ps4dp7"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((luajit-major+minor ,(version-major+minor (package-version lua)))
                (package-lua-resty (lambda (input output)
                                     (mkdir-p (string-append output "/lib/lua/" luajit-major+minor))
                                     (copy-recursively (string-append input "/lib/resty")
                                                       (string-append output "/lib/lua/" luajit-major+minor  "/resty"))
                                     (symlink (string-append output "/lib/lua/" luajit-major+minor "/resty")
                                              (string-append output "/lib/resty")))))
           (package-lua-resty (assoc-ref %build-inputs "source")
                              (assoc-ref %outputs "out")))
         #t)))
    (home-page "https://github.com/openresty/lua-resty-shell")
    (synopsis "Lua module for nonblocking system shell command executions")
    (description "This package provides Lua module for nonblocking system
shell command executions.")
    (license license:bsd-3)))

(define-public nginx-openresty
  (package
    (name "nginx-openresty")
    (version "0.0.1")
    (source #f)
    (inputs
     `(("bash" ,bash-minimal)
       ("nginx-lua" ,nginx)
       ("lua-resty-core" ,lua-resty-core)
       ("lua-resty-lrucache" ,lua-resty-lrucache)))
    (build-system trivial-build-system)
    (outputs '("out" "lua-resty-core" "lua-resty-lrucache"))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (mkdir-p (string-append %output "/sbin"))
         (let ((script (string-append %output "/sbin/nginx")))
           (with-output-to-file script
             (lambda ()
               (format #t (string-append "#!~a/bin/bash" (assoc-ref %build-inputs "bash")))
               (format #t "export LUA_PATH=~{~a/lib/?.lua:~}~%"
                       (assoc-ref %build-inputs "lua-resty-core")
                       (assoc-ref %build-inputs "lua-resty-lrucache"))
               (format #t "exec -a ~s ~a/sbin/nginx ~s~%"
                       "$0" (assoc-ref %build-inputs "nginx-lua") "$@")))
           (chmod script #o755))
         #t)))
    (home-page "https://github.com/openresty/")
    (synopsis "NGINX wrapper to run it with Lua support")
    (description
     "This package provides NGINX wrapper to run it with Lua support")
    (license license:expat)))
