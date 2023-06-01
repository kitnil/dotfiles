TESTS =						\
  tests/connect.bats				\
  tests/executables.bats			\
  tests/mail.bats				\
  tests/mjru.bats				\
  tests/guix.bats				\
  tests/ssh-mjru.bats				\
  tests/ssh-home.bats

.PHONY: all
all: guix.wugi.info ws1.wugi.info spb.wugi.info vm1.wugi.info vm2.wugi.info vm3.wugi.info vm4.wugi.info

.PHONY: clean-guile
clean-guile:
	rm -rf $(HOME)/.cache/guile/ccache

.PHONY: clean
clean: clean-guile
	rm -rf test-tmp
	rm -f dotfiles/nix/result

.PHONY: check
check:
	mkdir test-tmp
	gpg --quiet --decrypt dhall/ssh/ssh.dhall.gpg | dhall text > test-tmp/config
	bats $(TESTS)

.PHONY: benchmark
benchmark:
	emacs --eval "(progn (with-current-buffer (get-buffer \"*Benchmark Init Results Tabulated*\") (princ (buffer-substring-no-properties (point-min) (point-max)) #'external-debugging-output)) (kill-emacs))"

MODULES = dotfiles/guixsd/modules
HOSTNAME = $(shell hostname)

QEMU_FLAGS =					\
  -vnc :22					\
  -daemonize					\
  -m 4096					\
  -smp 2					\
  -nic user,model=virtio-net-pci,hostfwd=tcp::10022-:22

.PHONY: dotfiles/system/vm-image-jenkins.tmpl
dotfiles/system/vm-image-jenkins.tmpl:
	$(shell guix system vm -L $(MODULES) --no-offload dotfiles/system/vm-image-jenkins.tmpl) $(QEMU_FLAGS)

.PHONY: dotfiles/system/vm-image-stumpwm.tmpl
dotfiles/system/vm-image-stumpwm.tmpl:
	$(shell guix system vm -L $(MODULES) --no-offload dotfiles/system/vm-image-stumpwm.tmpl) $(QEMU_FLAGS)

.PHONY: extension-graph
extension-graph:
	guix system -L $(MODULES) extension-graph dotfiles/guixsd/guixsd.scm | xdot -

.PHONY: shepherd-graph
shepherd-graph:
	guix system -L $(MODULES) shepherd-graph dotfiles/guixsd/guixsd.scm | xdot -

.PHONY: configure
configure:
	./configure

dotfiles/guile/ssh.txt: dotfiles/guile/ssh.scm
	guile dotfiles/guile/ssh.scm > dotfiles/guile/ssh.txt

.PHONY:
decrypt:
	gpg --quiet --decrypt dotfiles/guixsd/modules/home/config/openssh.scm.gpg > dotfiles/guixsd/modules/home/config/openssh.scm

.PHONY: dotfiles/guixsd/home/guixsd.scm
dotfiles/guixsd/home/guixsd.scm:
	guix home -L dotfiles/guixsd/modules build dotfiles/guixsd/home/guixsd.scm

.PHONY: dotfiles/nix/flake.lock
dotfiles/nix/flake.lock:
	sh -c 'set -e; cd dotfiles/nix || exit 1; nix flake lock --update-input nixpkgs'

.PHONY: dotfiles/nix/flake.nix
dotfiles/nix/flake.nix:
	rm -rf $(HOME)/.cache/nix
	sh -c 'set -e; cd dotfiles/nix || exit 1; ./flake.nix'

.PHONY: dotfiles/dns/flake.nix
dotfiles/dns/flake.nix:
	sh -c 'set -e; cd dotfiles/dns || exit 1; ./flake.nix'

.PHONY: dotfiles/nix/nix.conf
dotfiles/nix/nix.conf:
	sudo mkdir -p /etc/nix
	sudo install -m644 dotfiles/nix/nix.conf /etc/nix/nix.conf

.PHONY: dotfiles/guixsd/machines.scm
dotfiles/guixsd/machines.scm:
	sudo install -m644 dotfiles/guixsd/machines.scm /etc/guix

.PHONY: dot_config/transmission/settings.json.gpg
dot_config/transmission/settings.json.gpg:
	gpg --decrypt dot_config/transmission/settings.json.gpg > $(HOME)/.config/transmission-daemon/settings.json

.PHONY: dot_config/espanso/user/censor.yml.gpg
dot_config/espanso/user/censor.yml.gpg:
	gpg --decrypt dot_config/espanso/user/censor.yml.gpg > $(HOME)/.config/espanso/user/censor.yml

.PHONY: dotfiles/mjru/intr.nix
dotfiles/mjru/intr.nix:
	dotfiles/mjru/intr.nix > dotfiles/mjru/intr.json

.PHONY: install
install: decrypt dotfiles/guixsd/machines.scm dotfiles/nix/nix.conf
	dot_local/bin/executable_gpg-unlock > /dev/null
	update-desktop-database $(HOME)/.local/share/applications
	mkdir -p $(HOME)/.config/mpv/scripts
	ln -sf $(HOME)/.nix-profile/share/mpv/scripts/notify-send.lua $(HOME)/.config/mpv/scripts/notify-send.lua
	install --mode=755 dotfiles/scripts/guix-channels-update $(HOME)/bin
	install --mode=755 dotfiles/scripts/guix-ci $(HOME)/bin
	install --mode=755 dotfiles/scripts/guix-package-version $(HOME)/bin
	install --mode=755 dotfiles/scripts/guix-profile-to-manifest $(HOME)/bin
	install --mode=755 dotfiles/scripts/maintenance $(HOME)/bin
	install --mode=755 dotfiles/scripts/sshrc $(HOME)/bin
	gpg --decrypt dotfiles/emacs/mjru-network.gpg > $(HOME)/.emacs.d/modules/mjru-network.el
	ln -sf $(HOME)/.Xresources $(HOME)/.Xdefaults
	install -Dm644 dotfiles/guile/pass.scm $(HOME)/.config/guile/pass.scm
	install -Dm644 dotfiles/guile/config.scm $(HOME)/.config/guile/config.scm
	guix home --load-path=$(PWD)/dotfiles/guixsd/modules reconfigure --no-substitutes dotfiles/guixsd/home/$(HOSTNAME).scm

.PHONY: guile-ihs
guile-ihs:
	guix environment --manifest=dotfiles/manifests/majordomo.scm -- sh -c 'type -p ihs'

.PHONY: deploy
deploy:
	guix deploy -L $(MODULES) dotfiles/guixsd/deploy.scm

.PHONY: dotfiles/channels-current.scm
dotfiles/channels-current.scm: clean-guile
	 GUILE_LOAD_PATH="${HOME}/.local/share/chezmoi/dotfiles/guixsd/modules:${GUILE_LOAD_PATH}" GUILE_AUTO_COMPILE=0 dot_local/bin/executable_guix-latest -L dotfiles/guixsd/modules --channels=dotfiles/channels-current.scm dotfiles/manifests/desktop.scm dotfiles/manifests/emacs.scm dotfiles/manifests/guix-collection.scm dotfiles/manifests/wigust.scm dotfiles/guixsd/guixsd.scm

.PHONY: dotfiles/packer/build.scm
dotfiles/packer/build.scm:
	sh -c 'cd dotfiles/packer; guix build -f build.scm'

.PHONY: guix.wugi.info
guix.wugi.info:
	guix system build -L $(MODULES) dotfiles/guixsd/guixsd.scm

.PHONY: ws1.wugi.info
ws1.wugi.info:
	guix system build -L $(MODULES) dotfiles/guixsd/ws1.wugi.info.scm

.PHONY: vm1.wugi.info
vm1.wugi.info:
	guix system build -L $(MODULES) dotfiles/guixsd/vm1.wugi.info.scm

.PHONY: add
add:
	cp $(HOME)/.emacs dot_emacs

.PHONY: github
github:
	make --directory=dotfiles/maintenance/github

.PHONY: gitlab
gitlab:
	make --directory=dotfiles/maintenance/gitlab

.PHONY: home
home:
	herd stop root
	rm -f /run/user/1000/shepherd/socket
	make install

guix_repository=$(HOME)/src/git.savannah.gnu.org/git/guix

sw4-mr11-container = $(shell set -e; cd $(guix_repository); ./pre-inst-env  guix pack -f docker-layered -S /bin=bin -L "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/modules" -e '(@ (packages h3c) state-to-vc-sw4-mr11)')
.PHONY: sw4-mr11
sw4-mr11:
	skopeo copy --insecure-policy docker-archive\:$(sw4-mr11-container) docker://docker-registry.wugi.info/monitoring/sw4-mr11

sw4-mr12-container = $(shell set -e; cd $(guix_repository); ./pre-inst-env  guix pack -f docker-layered -S /bin=bin -L "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/modules" -e '(@ (packages h3c) state-to-vc-sw4-mr12)')
.PHONY: sw4-mr12
sw4-mr12:
	skopeo copy --insecure-policy docker-archive\:$(sw4-mr12-container) docker://docker-registry.wugi.info/monitoring/sw4-mr12

sw4-mr13-container = $(shell set -e; cd $(guix_repository); ./pre-inst-env  guix pack -f docker-layered -S /bin=bin -L "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/modules" -e '(@ (packages h3c) state-to-vc-sw4-mr13)')
.PHONY: sw4-mr13
sw4-mr13:
	skopeo copy --insecure-policy docker-archive\:$(sw4-mr13-container) docker://docker-registry.wugi.info/monitoring/sw4-mr13

sw4-mr14-container = $(shell set -e; cd $(guix_repository); ./pre-inst-env  guix pack -f docker-layered -S /bin=bin -L "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/modules" -e '(@ (packages h3c) state-to-vc-sw4-mr14)')
.PHONY: sw4-mr14
sw4-mr14:
	skopeo copy --insecure-policy docker-archive\:$(sw4-mr14-container) docker://docker-registry.wugi.info/monitoring/sw4-mr14
