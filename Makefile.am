DECRYPT_TARGETS =					\
  guix/dot_config/espanso/user/censor.yml		\
  guix/dot_config/transmission/settings.json		\
  guix/dot_config/wayvnc/config				\
  guix/dot_config/wayvnc/tls_cert.pem			\
  guix/dot_config/wayvnc/tls_key.pem			\
  guix/dotfiles/etc/ssl/ca.pem				\
  guix/dotfiles/etc/yggdrasil-private.conf		\
  guix/dotfiles/guixsd/exim/dkim_rsa.private		\
  guix/private_dot_emacs.d/modules/mjru-network.el	\
  guix/wugi/etc/mjru/intr.json				\
  guix/wugi/home/config/openssh.scm			\
  nix/yggdrasil/yggdrasil.conf

.PHONY: all
all: $(foreach secret,$(DECRYPT_TARGETS),$(secret))

SUBSTITUTE_URLS ?= "https://bordeaux.guix.gnu.org https://mirrors.sjtug.sjtu.edu.cn/guix https://substitutes.nonguix.org"

LOAD_PATH=$(abs_top_srcdir)/guix

PASSWORD_STORE="$(abs_top_srcdir)/.password-store"
$(foreach secret,$(DECRYPT_TARGETS),$(secret)):
	mkdir -p $$(dirname $@)
	$(GPG) --output $@ --decrypt $(PASSWORD_STORE)/dotfiles/$@.gpg

.PHONY: clean-guile
clean-guile:
	rm -rf $(HOME)/.cache/guile/ccache

.PHONY: clean-nix
clean-nix:
	rm -rf $(HOME)/.cache/nix

.PHONY: clean-local
clean-local:
	$(GIT) clean -xdf

QEMU_FLAGS =						\
  -vnc :22						\
  -daemonize						\
  -m 4096						\
  -smp 2						\
  -nic user,model=virtio-net-pci,hostfwd=tcp::10022-:22

GUIX_SYSTEM_VM_CONFIGURATIONS =			\
  guixsd					\
  jenkins					\
  stumpwm

define guix-system-vm-arguments
system vm --load-path="$(LOAD_PATH)" --no-offload $(1)
endef

$(foreach configuration,$(GUIX_SYSTEM_VM_CONFIGURATIONS),guix-system-vm-configuration-$(configuration)):
	$(GUIX) $(call guix-system-vm-arguments,guix/dotfiles/system/vm-image-$(subst guix-system-vm-configuration-,,$@).tmpl)

.PHONY: guix/dotfiles/scripts/nix-ssh-known-hosts-to-file.scm
guix/dotfiles/scripts/nix-ssh-known-hosts-to-file.scm:
	mkdir -p private_dot_ssh
	$(shell $(GUIX) build -f guix/dotfiles/scripts/nix-ssh-known-hosts-to-file.scm)/bin/run.scm > private_dot_ssh/known_hosts2

.PHONY: nix/flake.lock
nix/flake.lock:
	sh -c 'set -e; cd nix || exit 1; nix flake lock --update-input nixpkgs'

.PHONY: nix/flake.nix
nix/flake.nix:
	sh -c 'set -e; cd nix || exit 1; ./flake.nix'

.PHONY: guix/dotfiles/dns/flake.nix
guix/dotfiles/dns/flake.nix:
	sh -c 'set -e; cd guix/dotfiles/dns || exit 1; ./flake.nix'

.PHONY: nix/firefox/generated-firefox-addons.nix
nix/firefox/generated-firefox-addons.nix:
	mozilla-addons-to-nix nix/firefox/addons.json nix/firefox/generated-firefox-addons.nix

.PHONY: guix/dotfiles/mjru/intr.nix
guix/dotfiles/mjru/intr.nix:
	guix/dotfiles/mjru/intr.nix > guix/wugi/etc/mjru/intr.json

.PHONY: deploy
deploy:
	$(GUIX) deploy --load-path="$(LOAD_PATH)" guix/dotfiles/guixsd/deploy.scm

.PHONY: guix/dotfiles/packer/build.scm
guix/dotfiles/packer/build.scm:
	sh -c 'cd guix/dotfiles/packer; $(GUIX) build -f build.scm'

GUIX_SYSTEM_CONFIGURATIONS =			\
  docker-image					\
  guixsd					\
  libvirt					\
  notebook					\
  pc0						\
  pc0-guix-workstation				\
  vm1						\
  vm2						\
  vm-guixsd					\
  wireless					\
  workstation

define guix-pull
$(GUIX) pull "--channels=$(abs_top_srcdir)/guix/wugi/etc/guix/channels/$(1).scm" --allow-downgrades --cores=$(shell nproc) --substitute-urls=$(SUBSTITUTE_URLS)
endef

$(foreach configuration,$(GUIX_SYSTEM_CONFIGURATIONS),guix-pull-$(configuration)):
	$(call guix-pull,$(call subst,guix-pull-,,$@))

$(foreach configuration,$(GUIX_SYSTEM_CONFIGURATIONS),root-guix-pull-$(configuration)):
	sudo -i $(call guix-pull,$(call subst,root-guix-pull-,,$@))

define guix-time-machine
$(GUIX) time-machine --channels="$(abs_top_srcdir)/guix/wugi/etc/guix/channels/$(1).scm" --substitute-urls=$(SUBSTITUTE_URLS)
endef

define guix-build-expression
build --load-path="$(LOAD_PATH)" --substitute-urls=$(SUBSTITUTE_URLS) --expression="((@ (wugi system $(1)) %$(2)))"
endef

define guix-system-reconfigure-expression
system reconfigure --load-path="$(LOAD_PATH)" --substitute-urls=$(SUBSTITUTE_URLS) --expression="((@ (wugi system $(1)) %$(2)))" $(ARGS)
endef

define guix-image-expression
system image --load-path="$(LOAD_PATH)" --substitute-urls=$(SUBSTITUTE_URLS) --max-layers=100 --image-type=docker --network --expression="((@ (wugi system $(1)) %$(2)))"
endef

$(foreach configuration,$(GUIX_SYSTEM_CONFIGURATIONS),guix-system-build-$(configuration)):
	$(GUIX) $(call guix-build-expression,$(call subst,guix-system-build-,,$@),$(call subst,guix-system-build-,,$@))

$(foreach configuration,$(GUIX_SYSTEM_CONFIGURATIONS),sudo-guix-system-build-$(configuration)):
	sudo -i $(call guix-build-expression,$(call subst,sudo-guix-system-build-,,$@),$(call subst,sudo-guix-system-build-,,$@))

$(foreach configuration,$(GUIX_SYSTEM_CONFIGURATIONS),guix-system-reconfigure-$(configuration)):
	sudo -i $(GUIX) $(call guix-system-reconfigure-expression,$(call subst,guix-system-reconfigure-,,$@),$(call subst,guix-system-reconfigure-,,$@))

$(foreach configuration,$(GUIX_SYSTEM_CONFIGURATIONS),guix-time-machine-system-image-$(configuration)):
	container=$$($(call guix-time-machine,$(call subst,guix-time-machine-system-image-,,$@)) -- $(call guix-image-expression,$(call subst,guix-time-machine-system-image-,,$@),$(call subst,guix-time-machine-system-image-,,$@)))
	IMG=$(container_registry)/library/$(call subst,guix-time-machine-system-image-,,$@):$$(echo $(GIT_COMMIT) | cut -c -8)-$$(date +%s)
	$(SKOPEO) copy docker-archive\:$$container docker://$$IMG
	echo $$IMG

$(foreach configuration,$(GUIX_SYSTEM_CONFIGURATIONS),guix-time-machine-system-build-$(configuration)):
	$(call guix-time-machine,$(call subst,guix-time-machine-system-build-,,$@)) -- $(call guix-build-expression,$(call subst,guix-time-machine-system-build-,,$@),$(call subst,guix-time-machine-system-build-,,$@))

define system-shepherd-graph
system shepherd-graph --load-path="$(LOAD_PATH)" --substitute-urls=$(SUBSTITUTE_URLS) --expression="((@ (wugi system $(1)) %$(2)))"
endef

$(foreach configuration,$(GUIX_SYSTEM_CONFIGURATIONS),guix-system-shepherd-graph-$(configuration)):
	$(GUIX) $(call system-shepherd-graph,$(call subst,guix-system-shepherd-graph-,,$@),$(call subst,guix-system-shepherd-graph-,,$@))

define system-extension-graph
extension-graph --load-path="$(LOAD_PATH)" --substitute-urls=$(SUBSTITUTE_URLS) --expression="((@ (wugi system $(1)) %$(2)))"
endef

$(foreach configuration,$(GUIX_SYSTEM_CONFIGURATIONS),guix-system-extension-graph-$(configuration)):
	$(GUIX) $(call system-extension-graph,$(call subst,guix-system-extension-graph-,,$@),$(call subst,guix-system-extension-graph-,,$@))

define guix-home-expression
home $1 --load-path="$(LOAD_PATH)" --substitute-urls=$(SUBSTITUTE_URLS) -e "((@ (wugi home config $(subst -home-environment,,$2)) %$(3)))"
endef

$(foreach configuration,$(GUIX_SYSTEM_CONFIGURATIONS),guix-home-build-$(configuration)): $(DECRYPT_TARGETS)
	$(GUIX) $(call guix-home-expression,build,$(call subst,guix-home-build-,,$@)-home-environment,$(call subst,guix-home-build-,,$@)-home-environment) $(ARGS)

$(foreach configuration,$(GUIX_SYSTEM_CONFIGURATIONS),guix-time-machine-home-build-$(configuration)): $(DECRYPT_TARGETS)
	$(call guix-time-machine,$(call subst,guix-time-machine-home-build-,,$@)) -- $(call guix-home-expression,build,$(call subst,guix-time-machine-home-build-,,$@)-home-environment,$(call subst,guix-time-machine-home-build-,,$@)-home-environment)

$(foreach configuration,$(GUIX_SYSTEM_CONFIGURATIONS),guix-home-reconfigure-$(configuration)): $(DECRYPT_TARGETS)
	$(GUIX) $(call guix-home-expression,reconfigure,$(call subst,guix-home-reconfigure-,,$@)-home-environment,$(call subst,guix-home-reconfigure-,,$@)-home-environment) --allow-downgrades $(ARGS)

$(foreach configuration,$(GUIX_SYSTEM_CONFIGURATIONS),guix-time-machine-home-reconfigure-$(configuration)):
	$(call guix-time-machine,$(call subst,guix-time-machine-home-reconfigure-,,$@)) -- $(call guix-home-expression,reconfigure,$(call subst,guix-time-machine-home-reconfigure-,,$@)-home-environment,$(call subst,guix-time-machine-home-reconfigure-,,$@)-home-environment) --allow-downgrades

define guix-build-manifest
build --load-path="$(LOAD_PATH)" --expression="((@ (wugi manifests $(subst $(1),,$(2))) %$(subst $(1),,$(2)-manifest)))"
endef

$(foreach configuration,$(GUIX_SYSTEM_CONFIGURATIONS),guix-build-manifest-$(configuration)):
	$(GUIX) $(call guix-build-manifest,guix-build-manifest-,$@)

$(foreach configuration,$(GUIX_SYSTEM_CONFIGURATIONS),guix-time-machine-build-manifest-$(configuration)):
	$(call guix-time-machine,$(call subst,guix-time-machine-build-manifest-,,$@)) -- $(call guix-build-manifest,guix-time-machine-build-manifest-,$(call subst,guix-time-machine-build-manifest-,,$@))

.PHONY: github
github:
	make --directory=guix/dotfiles/maintenance/github

.PHONY: gitlab
gitlab:
	make --directory=guix/dotfiles/maintenance/gitlab

.PHONY: home
home:
	herd stop root
	rm -f /run/user/1000/shepherd/socket
	make install

STATE_TO_VC_HOSTNAMES =				\
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

container_registry=harbor.home.wugi.info

.ONESHELL:
$(STATE_TO_VC_HOSTNAMES):
	set -o nounset -o errexit -o pipefail
	commit_8=$$(echo $(GIT_COMMIT) | cut -c -8)
	container=$$($(guix_repository)/pre-inst-env guix pack -f docker-layered -S /bin=bin --load-path="$(LOAD_PATH)" -e '(@ (packages networking) state-to-vc-$@)')
	$(SKOPEO) copy --insecure-policy docker-archive\:$$container docker://$(container_registry)/monitoring/$@:$$commit_8
	$(GUIX) gc --delete $$container
	cd $(HOME)/src/gitlab.intr/cd/state-to-git/apps/*/state-to-git-$@
	nix develop git+https://gitlab.intr/nixos/kubernetes --command kustomize edit set image $(container_registry)/monitoring/$@:$$commit_8
	if ! $(GIT) commit --message="apps: $$(basename $$(dirname $$(pwd))): state-to-git-$@: Update image to $$commit_8." kustomization.yaml
	then
	    :
	fi

state-to-vc-containers: $(STATE_TO_VC_HOSTNAMES)

.ONESHELL:
util-linux-with-udev:
	set -o nounset -o errexit -o pipefail -o xtrace
	commit_8=$$(echo $(GIT_COMMIT) | cut -c -8)
	container=$$($(GUIX) pack -f docker --load-path="$(LOAD_PATH)" --max-layers=100 -S /bin=bin util-linux-with-udev bash coreutils guile guix-refresh.sh)
	$(SKOPEO) copy --insecure-policy docker-archive\:$$container docker://$(container_registry)/library/$@:$$commit_8
	$(GUIX) gc --delete $$container
	cd apps/base/maintenance-guix-refresh-gita
	nix develop git+https://gitlab.intr/nixos/kubernetes --command kustomize edit set image $(container_registry)/library/$@:$$commit_8
	if ! $(GIT) commit --message="apps: $$(basename $$(dirname $$(pwd))): maintenance-guix-refresh-gita: Update image to $$commit_8." kustomization.yaml
	then
	    :
	fi

.ONESHELL:
skopeo-umoci:
	set -o nounset -o errexit -o pipefail -o xtrace
	commit_8=$$(echo $(GIT_COMMIT) | cut -c -8)
	container=$$($(GUIX) pack -f docker --load-path="$(LOAD_PATH)" --max-layers=100 -S /bin=bin -S /etc=etc bash coreutils skopeo umoci nss-certs)
	$(SKOPEO) copy --insecure-policy docker-archive\:$$container docker://$(container_registry)/library/$@:$$commit_8
	$(GUIX) gc --delete $$container

.ONESHELL:
runc:
	set -o nounset -o errexit -o pipefail -o xtrace
	commit_8=$$(echo $(GIT_COMMIT) | cut -c -8)
	container=$$($(GUIX) pack -f docker --load-path="$(LOAD_PATH)" --max-layers=100 -S /bin=bin -S /sbin=sbin util-linux-with-udev bash coreutils runc)
	$(SKOPEO) copy --insecure-policy docker-archive\:$$container docker://$(container_registry)/library/$@:$$commit_8
	$(GUIX) gc --delete $$container

.ONESHELL:
guix-entrypoint:
	set -o nounset -o errexit -o pipefail -o xtrace
	commit_8=$$(echo $(GIT_COMMIT) | cut -c -8)
	container=$$($(GUIX) pack -f docker --load-path="$(LOAD_PATH)" --max-layers=100 -S /bin=bin -S /sbin=sbin util-linux-with-udev bash coreutils iproute2)
	$(SKOPEO) copy --insecure-policy docker-archive\:$$container docker://$(container_registry)/library/$@:$$commit_8
	$(GUIX) gc --delete $$container

.ONESHELL:
kubeedge-certs:
	set -o nounset -o errexit -o pipefail -o xtrace
	commit_8=$$(echo $(GIT_COMMIT) | cut -c -8)
	container=$$($(GUIX) pack -f docker --load-path="$(LOAD_PATH)" --max-layers=100 -S /bin=bin util-linux-with-udev bash coreutils guile openssl)
	$(SKOPEO) copy --insecure-policy docker-archive\:$$container docker://$(container_registry)/library/$@:$$commit_8
	$(GUIX) gc --delete $$container

.ONESHELL:
haproxy:
	set -o nounset -o errexit -o pipefail -o xtrace
	commit_8=$$(echo $(GIT_COMMIT) | cut -c -8)
	container=$$($(GUIX) pack -f docker --max-layers=100 -S /sbin=sbin haproxy)
	$(SKOPEO) copy --insecure-policy docker-archive\:$$container docker://$(container_registry)/library/$@:$$commit_8

.ONESHELL:
isc-dhcp:
	set -o nounset -o errexit -o pipefail -o xtrace
	commit_8=$$(echo $(GIT_COMMIT) | cut -c -8)
	container=$$($(GUIX) system image --load-path=/home/oleg/src/cgit.wugi.info/wigust/dotfiles/guix/wugi --max-layers=100 -t docker --network guix/dotfiles/guixsd/docker-image-isc-dhcp.scm)
	$(SKOPEO) copy docker-archive\:$$container docker://$(container_registry)/library/$@:$$commit_8

.ONESHELL:
mumble:
	set -o nounset -o errexit -o pipefail -o xtrace
	commit_8=$$(echo $(GIT_COMMIT) | cut -c -8)
	container=$$($(GUIX) time-machine --channels=guix/dotfiles/channels-guix-mumble.scm -- system image --max-layers=100 -t docker --network guix/wugi/system/docker-image-mumble.scm)
	$(SKOPEO) copy docker-archive\:$$container docker://$(container_registry)/library/$@:$$commit_8

.ONESHELL:
pc0-manifest:
	set -o nounset -o errexit -o pipefail -o xtrace
	$(GUIX) time-machine --channels=guix/wugi/etc/guix/channels/workstation.scm -- build --load-path="$(LOAD_PATH)" -m wugi/manifests/pc0.scm --substitute-urls=$(SUBSTITUTE_URLS)

.ONESHELL:
guix-image-builder: $(DECRYPT_TARGETS)
	set -o nounset -o errexit -o pipefail -o xtrace
	commit_8=$$(echo $(GIT_COMMIT) | cut -c -8)
	container=$$($(GUIX) time-machine --channels=guix/dotfiles/channels-current-guix-image-builder.scm -- system image --substitute-urls=$(SUBSTITUTE_URLS) --max-layers=100 -t docker --network ~/src/cgit.wugi.info/wigust/dotfiles/guix/wugi/system/guix-image-builder.scm)
	$(SKOPEO) copy docker-archive\:$$container docker://$(container_registry)/library/$@:$$commit_8

nix-update-upstream-inputs:
	$(MAKE) -C nix nur

nix-update-inputs:
	$(MAKE) -C nix/container-systemd dotfiles-home-manager
	$(MAKE) -C nix/container-systemd-taskexecutor original
	$(MAKE) -C nix/container-systemd-nixos-majordomo original
	$(MAKE) -C nix/container-systemd-nixos-workstation original
	$(MAKE) -C nix/container-systemd-nixos-workstation-guixsd original
	$(MAKE) -C nix/container-systemd-nixos-workstation-pc0 original
	$(MAKE) -C nix/container-systemd-nixos-zapret original

container-systemd-taskexecutor: $(DECRYPT_TARGETS)
	$(MAKE) -C nix/container-systemd-taskexecutor

.ONESHELL:
workstation-controller:
	set -o nounset -o errexit -o pipefail -o xtrace
	commit_8=$$(echo $(GIT_COMMIT) | cut -c -8)
	$(MAKE) -C src/go/workstation-controller docker-build IMG=$(container_registry)/library/$@:$$commit_8
	$(MAKE) -C src/go/workstation-controller docker-push IMG=$(container_registry)/library/$@:$$commit_8

.PHONY: dotfiles-update-commit
dotfiles-update-commit:
	$(GUIX) shell guile guile-git guile-gcrypt guile-json yq -- guix/dot_local/bin/dotfiles-update-commit

.ONESHELL:
archlinux:
	$(MAKE) -C apps/base/kaniko-archlinux

.PHONY: known_hosts
known_hosts:
	guix/dotfiles/scripts/nix-ssh-known-hosts-to-file.scm

runc-nix-builder-nixos-rebuild-switch-flake:
	$(MAKE) -C nix/container-systemd-builder nixos-rebuild-switch-flake
