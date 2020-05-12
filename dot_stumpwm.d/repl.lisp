(in-package :stumpwm)

(defcommand repl-groovy () ()
  (term-shell-command "groovysh" :scrollbar t))

(defcommand repl-guile () ()
  (term-shell-command "guile" :scrollbar t))

(defcommand repl-guix () ()
  (term-shell-command "guix environment --pure --ad-hoc coreutils findutils guile-next guile3.0-colorized guile3.0-readline -- ~/.config/guix/current/bin/guix repl"
                      :scrollbar t))

(defcommand repl-gdb () ()
  (term-shell-command "gdb" :scrollbar t :title "repl-gdb"))

(defcommand repl-php () ()
  (term-shell-command "php -a" :scrollbar t :title "repl-php"))

(defcommand repl-python () ()
  (term-shell-command "python3" :scrollbar t :title "repl-python"))

(defcommand repl-r () ()
  (term-shell-command "R" :scrollbar t :title "repl-r"))

(defcommand repl-racket () ()
  (term-shell-command "racket" :scrollbar t :title "repl-racket"))

(defcommand repl-nix () ()
  (term-shell-command "nix repl '<nixpkgs>'" :scrollbar t :title "repl-nix"))

(defcommand repl-nix-20-03 () ()
  (term-shell-command "nix repl /home/oleg/.nix-defexpr/channels/nixos-20-03"
                      :scrollbar t :title "repl-nix-20-03"))

(defcommand repl-nix-unstable () ()
  (term-shell-command "nix repl /home/oleg/.nix-defexpr/channels/nixos-unstable"
                      :scrollbar t :title "repl-nix-unstable"))

(defcommand repl-emacs () ()
  (term-shell-command "emacs -nw -q -e 'ielm'" :scrollbar t :title "repl-emacs"))

(defcommand repl-kawa () ()
  (term-shell-command "guix environment --ad-hoc coreutils openjdk kawa rlwrap -- rlwrap kawa" :scrollbar t :title "repl-kawa"))

(defcommand repl-ocaml () ()
  (term-shell-command "guix environment --pure --ad-hoc ocaml -- ocaml" :scrollbar t :title "repl-ocaml"))

(defcommand repl-octave () ()
  (term-shell-command "octave" :scrollbar t :title "repl-octave"))

(defcommand repl-node () ()
  (term-shell-command "node" :scrollbar t :title "repl-node"))

(defcommand repl-sbcl () ()
  (term-shell-command "rlwrap sbcl" :scrollbar t :title "repl-sbcl"))

(defcommand repl-chez-scheme () ()
  (term-shell-command "guix environment --ad-hoc chez-scheme -- chez-scheme" :scrollbar t :title "repl-chez-scheme"))

(defcommand repl-chicken () ()
  (term-shell-command "rlwrap csi" :scrollbar t :title "repl-chicken"))

(defcommand repl-c () ()
  (term-shell-command "docker run --rm -it bic:latest" :scrollbar t :title "repl-c"))

(defcommand repl-hy () ()
  (term-shell-command "docker run --rm -it hylang:python3.5-buster" :scrollbar t :title "repl-hy"))

(defcommand repl-java () ()
  (term-shell-command "docker run --rm -it openjdk:9" :scrollbar t :title "repl-java"))

(defcommand repl-elm () ()
  (term-shell-command "guix environment --pure --ad-hoc elm-compiler node -- elm repl"
                      :scrollbar t :title "repl-elm"))

(defcommand repl-clisp () ()
  (term-shell-command "guix environment --pure --ad-hoc clisp -- clisp"
                      :scrollbar t :title "repl-clisp"))

(defcommand repl-erlang () ()
  (term-shell-command "guix environment --pure --ad-hoc erlang -- erl"
                      :scrollbar t :title "repl-erlang"))

(defcommand repl-lua () ()
  (term-shell-command "guix environment --pure --ad-hoc lua -- lua"
                      :scrollbar t :title "repl-lua"))

(defcommand repl-ghci () ()
  (term-shell-command "ghci" :scrollbar t :title "repl-ghci"))

(defcommand repl-go () ()
  (term-shell-command "gomacro" :scrollbar t :title "repl-go"))

(defcommand repl-resty () ()
  (term-shell-command (format nil (join '("docker" "run" "--entrypoint"
                                          "''" "--rm" "-it"
                                          "openresty/openresty:bionic" "bash" "-c" "~s"))
                              (join '("apt update" "apt install -y git"
                                      "luarocks install lua-resty-repl"
                                      "resty-repl")
                                    #\;))
                      :scrollbar t :title "repl-java"))

(defcommand repl-ruby () ()
  (term-shell-command "guix environment --pure --ad-hoc ruby -- irb"
                      :scrollbar t :title "repl-ruby"))

(defcommand repl-bash-pure () ()
  (term-shell-command "env -i \"$(command -v bash)\" --login --noprofile --norc"
                      :scrollbar t :title "repl-bash-pure"))

(defcommand repl-ansible (group) ((:string "Ansible inventory group: "))
  (term-shell-command (format nil "ansible-console ~a" group)
                      :scrollbar t :title "repl-ansible" :color 'dark))


;;;
;;; Swank
;;;

;; https://lists.gnu.org/archive/html/help-guix/2017-01/msg00033.html

(load (concat (getenv "HOME") "/.guix-profile/share/emacs/site-lisp/swank.asd"))

(require :swank)

(defcommand swank (port) ((:string "Port number: "))
  (sb-thread:make-thread
   (lambda ()
     (swank:create-server :port (parse-integer port) :dont-close t))
   :name "swank"))
(mapcar (lambda (func)
          (add-hook *start-hook* func))
        (list (lambda ()
                (swank "4005"))))
