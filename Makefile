TESTS =						\
  tests/connect.bats				\
  tests/executables.bats			\
  tests/mail.bats				\
  tests/mjru.bats				\
  tests/guix.bats				\
  tests/ssh-mjru.bats				\
  tests/ssh-home.bats

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

define guix-time-machine-arguments
guix time-machine -C dotfiles/channels-current.scm
endef

define guix-system-vm-arguments
system vm -L $(MODULES) --no-offload dotfiles/system/$(1)
endef

guix-system-vm-configurations =			\
  guixsd					\
  jenkins					\
  stumpwm

guix-system-vm-configuration-prefix := guix-system-vm-configuration-
$(foreach configuration,$(guix-system-vm-configurations),$(guix-system-vm-configuration-prefix)-$(configuration)):
	guix $(call guix-system-vm-arguments,$(guix-system-vm-configuration-prefix),$@)

time-machine-guix-system-vm-configuration-prefix = time-machine-guix-system-vm-configuration-
$(foreach configuration,$(guix-system-vm-configurations),$(time-machine-guix-system-vm-configuration-prefix)$(configuration)):
	$(call guix-time-machine-arguments) -- $(call guix-system-vm-arguments,$(subst $(time-machine-guix-system-vm-configuration-prefix),vm-image-,$@).tmpl)

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

.PHONY: dotfiles/scripts/nix-ssh-known-hosts-to-file.scm
dotfiles/scripts/nix-ssh-known-hosts-to-file.scm:
	$(shell guix build -f dotfiles/scripts/nix-ssh-known-hosts-to-file.scm)/bin/run.scm > $(HOME)/.ssh/known_hosts2

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
	guix home --load-path=dotfiles/guixsd/modules reconfigure --no-substitutes dotfiles/guixsd/home/$(HOSTNAME).scm

.PHONY: shepherd-restart
shepherd-restart:
	$(shell set +e; herd stop root)
	rm -f /run/user/$(UID)/shepherd/socket
	make install

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

guix-system-configurations =			\
  guixsd					\
  vm1.wugi.info					\
  vm2.wugi.info					\
  ws1.wugi.info

define guix-system-arguments
system build -L $(MODULES) dotfiles/guixsd/$(subst $(1),,$(2)).scm
endef

define guix-package-manifest-arguments
shell -L $(MODULES) --manifest=dotfiles/manifests/$(subst $(1),,$(2)).scm -- exit 0
endef

prefix := guix-system-configuration-
$(foreach configuration,$(guix-system-configurations),guix-system-configuration-$(configuration)):
	guix $(call guix-system-arguments,$(prefix),$@)

prefix := time-machine-guix-system-configuration-
$(foreach configuration,$(guix-system-configurations),time-machine-guix-system-configuration-$(configuration)):
	$(call guix-time-machine-arguments) -- $(call guix-system-arguments,$(prefix),$@)

prefix := guix-package-manifest-
$(foreach configuration,$(guix-system-configurations),guix-package-manifest-$(configuration)):
	$(call guix-package-manifest-arguments,$(prefix),$@)

prefix := time-machine-guix-package-manifest-
$(foreach configuration,$(guix-system-configurations),time-machine-guix-package-manifest-$(configuration)):
	$(call guix-time-machine-arguments) -- $(call guix-package-manifest-arguments,$(prefix),$@)

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

state-to-vc-hostnames =			\
  sr1-dh507-508					\
  sr1-mr13-14					\
  sw1-dh507					\
  sw1-dh508					\
  sw1-mr11					\
  sw1-mr12					\
  sw1-mr14					\
  sw2-dh507					\
  sw2-dh508					\
  sw2-mr12					\
  sw2-mr13					\
  sw2-mr14					\
  sw4-mr11					\
  sw4-mr12					\
  sw4-mr13					\
  sw4-mr14

guix_repository=$(HOME)/src/git.savannah.gnu.org/git/guix
container_registry=harbor.corp1.majordomo.ru

.ONESHELL:
$(state-to-vc-hostnames):
	set -o nounset -o errexit -o pipefail
	commit_8=$$(git rev-parse HEAD | cut -c -8)
	container=$$($(guix_repository)/pre-inst-env guix pack -f docker-layered -S /bin=bin -L dotfiles/guixsd/modules -e '(@ (packages networking) state-to-vc-$@)')
	skopeo copy --insecure-policy docker-archive\:$$container docker://$(container_registry)/monitoring/$@:$$commit_8
	guix gc --delete $$container
	cd $(HOME)/src/gitlab.intr/cd/state-to-git/apps/*/state-to-git-$@
	nix develop git+https://gitlab.intr/nixos/kubernetes --command kustomize edit set image $(container_registry)/monitoring/$@:$$commit_8
	if ! git commit --message="apps: $$(basename $$(dirname $$(pwd))): state-to-git-$@: Update image to $$commit_8." kustomization.yaml
	then
	    :
	fi

state-to-vc-containers: $(state-to-vc-hostnames)
