.PHONY: clean-guile
clean-guile:
	rm -rf $(HOME)/.cache/guile/ccache

.PHONY: clean-nix
clean-nix:
	rm -rf $(HOME)/.cache/nix

.PHONY: clean
clean: clean-guile clean-nix
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

HOSTNAME = $(shell hostname)

QEMU_FLAGS =					\
  -vnc :22					\
  -daemonize					\
  -m 4096					\
  -smp 2					\
  -nic user,model=virtio-net-pci,hostfwd=tcp::10022-:22

define guix-system-vm-arguments
system vm -L wugi --no-offload dotfiles/system/$(1)
endef

guix-system-vm-configurations =			\
  guixsd					\
  jenkins					\
  stumpwm

guix-system-vm-configuration-prefix := guix-system-vm-configuration-
$(foreach configuration,$(guix-system-vm-configurations),$(guix-system-vm-configuration-prefix)-$(configuration)):
	guix $(call guix-system-vm-arguments,$(guix-system-vm-configuration-prefix),$@)

.PHONY: extension-graph
extension-graph:
	guix system -L wugi extension-graph dotfiles/guixsd/guixsd.scm | xdot -

.PHONY: shepherd-graph
shepherd-graph:
	guix system -L wugi shepherd-graph dotfiles/guixsd/guixsd.scm | xdot -

.PHONY: configure
configure:
	./configure

dotfiles/guile/ssh.txt: dotfiles/guile/ssh.scm
	guile dotfiles/guile/ssh.scm > dotfiles/guile/ssh.txt

wugi/home/config/openssh.scm.gpg:
	gpg --quiet --decrypt wugi/home/config/openssh.scm.gpg > wugi/home/config/openssh.scm

.PHONY: dotfiles/scripts/nix-ssh-known-hosts-to-file.scm
dotfiles/scripts/nix-ssh-known-hosts-to-file.scm:
	mkdir -p private_dot_ssh
	$(shell guix build -f dotfiles/scripts/nix-ssh-known-hosts-to-file.scm)/bin/run.scm > private_dot_ssh/known_hosts2

.PHONY: dotfiles/nix/flake.lock
dotfiles/nix/flake.lock:
	sh -c 'set -e; cd dotfiles/nix || exit 1; nix flake lock --update-input nixpkgs'

.PHONY: dotfiles/nix/flake.nix
dotfiles/nix/flake.nix:
	sh -c 'set -e; cd dotfiles/nix || exit 1; ./flake.nix'

.PHONY: dotfiles/dns/flake.nix
dotfiles/dns/flake.nix:
	sh -c 'set -e; cd dotfiles/dns || exit 1; ./flake.nix'

.PHONY: dotfiles/nix/nix.conf
dotfiles/nix/nix.conf:
	sudo mkdir -p /etc/nix
	sudo install -m644 dotfiles/nix/nix.conf /etc/nix/nix.conf

.PHONY: dotfiles/nix/firefox/generated-firefox-addons.nix
dotfiles/nix/firefox/generated-firefox-addons.nix:
	mozilla-addons-to-nix dotfiles/nix/firefox/addons.json dotfiles/nix/firefox/generated-firefox-addons.nix

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
install: wugi/home/config/openssh.scm.gpg dotfiles/guixsd/machines.scm dotfiles/nix/nix.conf dotfiles/scripts/nix-ssh-known-hosts-to-file.scm
	dot_local/bin/gpg-unlock > /dev/null
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
	guix home --load-path=wugi reconfigure dotfiles/guixsd/home/$(HOSTNAME).scm
	install -Dm644 private_dot_ssh/known_hosts2 $(HOME)/.ssh/known_hosts2

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
	guix deploy -L wugi dotfiles/guixsd/deploy.scm

.PHONY: dotfiles/packer/build.scm
dotfiles/packer/build.scm:
	sh -c 'cd dotfiles/packer; guix build -f build.scm'

guix-system-configurations =			\
  guixsd					\
  notebook					\
  pc0					\
  vm1					\
  vm2					\
  workstation

define guix-system-arguments
guix time-machine --channels=guix/wugi/etc/guix/channels/$(subst $(1),,$(2)).scm -- build --load-path=guix -e "((@ (wugi system $(subst $(1),,$(2))) %$(subst $(1),,$(2))))"
endef

define guix-package-manifest-arguments
shell -L wugi --manifest=dotfiles/manifests/$(subst $(1),,$(2)).scm -- exit 0
endef

define guix-time-machine
guix time-machine "--channels=guix/wugi/etc/guix/channels/$(1).scm"
endef

define guix-build-expression
build --load-path=guix -e "((@ (wugi system $$system) %$(1)))"
endef

$(foreach configuration,$(guix-system-configurations),$(configuration)):
	$(call guix-system-arguments,guix-system-configuration-,$@)

$(foreach configuration,$(guix-system-configurations),time-machine-guix-system-configuration-$(configuration)):
	system=$(subst time-machine-guix-system-configuration-,,$(@)); \
	$(call guix-time-machine,$$system) -- $(call guix-build-expression,$$system)

$(foreach configuration,$(guix-system-configurations),guix-package-manifest-$(configuration)):
	$(call guix-package-manifest-arguments,guix-package-manifest-,$@)

$(foreach configuration,$(guix-system-configurations),time-machine-guix-package-manifest-$(configuration)):
	$(call guix-package-manifest-arguments,time-machine-guix-package-manifest-,$@)

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
	container=$$($(guix_repository)/pre-inst-env guix pack -f docker-layered -S /bin=bin -L wugi -e '(@ (packages networking) state-to-vc-$@)')
	skopeo copy --insecure-policy docker-archive\:$$container docker://$(container_registry)/monitoring/$@:$$commit_8
	guix gc --delete $$container
	cd $(HOME)/src/gitlab.intr/cd/state-to-git/apps/*/state-to-git-$@
	nix develop git+https://gitlab.intr/nixos/kubernetes --command kustomize edit set image $(container_registry)/monitoring/$@:$$commit_8
	if ! git commit --message="apps: $$(basename $$(dirname $$(pwd))): state-to-git-$@: Update image to $$commit_8." kustomization.yaml
	then
	    :
	fi

state-to-vc-containers: $(state-to-vc-hostnames)

container_registry=docker-registry.wugi.info
.ONESHELL:
util-linux-with-udev:
	set -o nounset -o errexit -o pipefail -o xtrace
	commit_8=$$(git rev-parse HEAD | cut -c -8)
	container=$$(guix pack -f docker -L wugi --max-layers=100 -S /bin=bin util-linux-with-udev bash coreutils guile guix-refresh.sh)
	skopeo copy --insecure-policy docker-archive\:$$container docker://$(container_registry)/library/$@:$$commit_8
	guix gc --delete $$container
	cd apps/base/maintenance-guix-refresh-gita
	nix develop git+https://gitlab.intr/nixos/kubernetes --command kustomize edit set image $(container_registry)/library/$@:$$commit_8
	if ! git commit --message="apps: $$(basename $$(dirname $$(pwd))): maintenance-guix-refresh-gita: Update image to $$commit_8." kustomization.yaml
	then
	    :
	fi

container_registry=docker-registry.wugi.info
skopeo-umoci:
.ONESHELL:
skopeo-umoci:
	set -o nounset -o errexit -o pipefail -o xtrace
	commit_8=$$(git rev-parse HEAD | cut -c -8)
	container=$$(guix pack -f docker -L wugi --max-layers=100 -S /bin=bin -S /etc=etc bash coreutils skopeo umoci nss-certs)
	skopeo copy --insecure-policy docker-archive\:$$container docker://$(container_registry)/library/$@:$$commit_8
	guix gc --delete $$container

container_registry=docker-registry.wugi.info
.ONESHELL:
runc:
	set -o nounset -o errexit -o pipefail -o xtrace
	commit_8=$$(git rev-parse HEAD | cut -c -8)
	container=$$(guix pack -f docker -L wugi --max-layers=100 -S /bin=bin -S /sbin=sbin util-linux-with-udev bash coreutils runc)
	skopeo copy --insecure-policy docker-archive\:$$container docker://$(container_registry)/library/$@:$$commit_8
	guix gc --delete $$container

container_registry=docker-registry.wugi.info
.ONESHELL:
haproxy:
	set -o nounset -o errexit -o pipefail -o xtrace
	commit_8=$$(git rev-parse HEAD | cut -c -8)
	container=$$(guix pack -f docker --max-layers=100 -S /sbin=sbin haproxy)
	skopeo copy --insecure-policy docker-archive\:$$container docker://$(container_registry)/library/$@:$$commit_8

container_registry=harbor.home.wugi.info
.ONESHELL:
isc-dhcp:
	set -o nounset -o errexit -o pipefail -o xtrace
	commit_8=$$(git rev-parse HEAD | cut -c -8)
	container=$$(guix system image --load-path=/home/oleg/.local/share/chezmoi/wugi --max-layers=100 -t docker --network dotfiles/guixsd/docker-image-isc-dhcp.scm)
	skopeo copy docker-archive\:$$container docker://$(container_registry)/library/$@:$$commit_8

container_registry=harbor.home.wugi.info
.ONESHELL:
mumble:
	set -o nounset -o errexit -o pipefail -o xtrace
	commit_8=$$(git rev-parse HEAD | cut -c -8)
	container=$$(guix time-machine --channels=dotfiles/channels-guix-mumble.scm -- system image --max-layers=100 -t docker --network dotfiles/guixsd/docker-image-mumble.scm)
	skopeo copy docker-archive\:$$container docker://$(container_registry)/library/$@:$$commit_8

container_registry=harbor.home.wugi.info
.ONESHELL:
guix-image-workstation: wugi/home/config/openssh.scm.gpg
	set -o nounset -o errexit -o pipefail -o xtrace
	IMG=$(container_registry)/library/$@:$$(git rev-parse --abbrev-ref HEAD)-$$(git rev-parse HEAD | cut -c -8)-$$(date +%s)
	container=$$(guix time-machine --channels=guix/wugi/etc/guix/channels/workstation.scm -- system image --load-path=guix --substitute-urls='https://bordeaux.guix.gnu.org https://substitutes.nonguix.org http://ci.guix.trop.in' --max-layers=100 -t docker --network -e '((@ (wugi system workstation) %workstation))')
	skopeo copy docker-archive\:$$container docker://$$IMG
	echo $$IMG

.ONESHELL:
pc0-manifest:
	set -o nounset -o errexit -o pipefail -o xtrace
	guix time-machine --channels=guix/wugi/etc/guix/channels/workstation.scm -- build --load-path=guix -m wugi/manifests/pc0.scm --substitute-urls='https://bordeaux.guix.gnu.org https://substitutes.nonguix.org http://ci.guix.trop.in'

container_registry=harbor.home.wugi.info
.ONESHELL:
guix-image-builder: wugi/home/config/openssh.scm.gpg
	set -o nounset -o errexit -o pipefail -o xtrace
	commit_8=$$(git rev-parse HEAD | cut -c -8)
	container=$$(guix time-machine --channels=dotfiles/channels-current-guix-image-builder.scm -- system image --substitute-urls='https://guix.wugi.info https://bordeaux.guix.gnu.org https://substitutes.nonguix.org http://ci.guix.trop.in' --max-layers=100 -t docker --network ~/.local/share/chezmoi/dotfiles/guixsd/guix-image-builder.scm)
	skopeo copy docker-archive\:$$container docker://$(container_registry)/library/$@:$$commit_8
	echo $(container_registry)/library/$@:$$commit_8

nix-update-inputs:
	$(MAKE) -C dotfiles/nix/container-systemd dotfiles-home-manager
	$(MAKE) -C dotfiles/nix/container-systemd-taskexecutor original

container-systemd-taskexecutor:
	$(MAKE) -C dotfiles/nix/container-systemd-taskexecutor

container_registry=harbor.home.wugi.info
.ONESHELL:
workstation-controller:
	set -o nounset -o errexit -o pipefail -o xtrace
	commit_8=$$(git rev-parse HEAD | cut -c -8)
	$(MAKE) -C src/go/workstation-controller docker-build IMG=$(container_registry)/library/$@:$$commit_8
	$(MAKE) -C src/go/workstation-controller docker-push IMG=$(container_registry)/library/$@:$$commit_8

.PHONY: dotfiles-update-commit
dotfiles-update-commit:
	guix shell guile guile-git guile-gcrypt guile-json yq -- dot_local/bin/dotfiles-update-commit

container_registry=harbor.home.wugi.info
.ONESHELL:
archlinux:
	$(MAKE) -C apps/base/kaniko-archlinux

.PHONY: all
all: dotfiles/scripts/nix-ssh-known-hosts-to-file.scm
