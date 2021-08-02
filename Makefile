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

.PHONY: clean
clean:
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

QEMU_FLAGS =					\
  -vnc :22					\
  -daemonize					\
  -m 4096					\
  -smp 2					\
  -nic user,model=virtio-net-pci,hostfwd=tcp::10022-:22

.PHONY: vm
vm:
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
install-ssh:
	gpg --quiet --decrypt dhall/ssh/ssh.dhall.gpg | dhall text > $(HOME)/.ssh/config

.PHONY: install
install: install-ssh
	bin/executable_gpg-unlock
	chezmoi apply
	update-desktop-database $(HOME)/.local/share/applications
	open-with-linux install
	mkdir -p $(HOME)/.config/mpv/scripts
	ln -sf $(HOME)/.nix-profile/share/mpv/scripts/notify-send.lua $(HOME)/.config/mpv/scripts/notify-send.lua
	sh -c 'cd dotfiles/idesk; ./idesk.scm'
	mkdir -p $(HOME)/.idesktop
	install --mode=644 dotfiles/idesk/dotfiles.lnk $(HOME)/.idesktop
	install --mode=644 dotfiles/idesk/guix.lnk $(HOME)/.idesktop
	install --mode=644 dotfiles/idesk/xterm.lnk $(HOME)/.idesktop
	install --mode=644 dotfiles/idesk/vm1.wugi.info.lnk $(HOME)/.idesktop
	install --mode=644 dotfiles/idesk/guix.vm.wugi.info.lnk $(HOME)/.idesktop
	install --mode=644 dotfiles/idesk/workstation.lnk $(HOME)/.idesktop
	install --mode=755 dotfiles/scripts/guix-channels-update $(HOME)/bin
	install --mode=755 dotfiles/scripts/guix-ci $(HOME)/bin
	install --mode=755 dotfiles/scripts/guix-package-version $(HOME)/bin
	install --mode=755 dotfiles/scripts/guix-profile-to-manifest $(HOME)/bin
	install --mode=755 dotfiles/scripts/maintenance $(HOME)/bin
	install --mode=755 dotfiles/scripts/rofi-mycli $(HOME)/bin
	install --mode=755 dotfiles/scripts/sshrc $(HOME)/bin
	sudo install -m644 dotfiles/guixsd/machines.scm /etc/guix
	gpg --decrypt dotfiles/emacs/mjru-network.gpg > $(HOME)/.emacs.d/modules/mjru-network.el
	ln -sf $(HOME)/.Xresources $(HOME)/.Xdefaults

.PHONY: guile-ihs
guile-ihs:
	guix environment --manifest=dotfiles/manifests/majordomo.scm -- sh -c 'type -p ihs'

.PHONY: deploy
deploy:
	guix deploy -L $(MODULES) dotfiles/guixsd/deploy.scm

.PHONY: latest
latest:
	guix-latest -L dotfiles/guixsd/modules --channels=dotfiles/channels-current.scm dotfiles/manifests/guixsd.scm dotfiles/guixsd/guixsd.scm

.PHONY: guix.wugi.info
guix.wugi.info:
	guix system build -L $(MODULES) dotfiles/guixsd/guixsd.scm

.PHONY: ws1.wugi.info
ws1.wugi.info:
	guix system build -L $(MODULES) dotfiles/guixsd/ws1.wugi.info.scm

.PHONY: spb.wugi.info
spb.wugi.info:
	guix system build -L $(MODULES) dotfiles/guixsd/spb.scm

.PHONY: vm1.wugi.info
vm1.wugi.info:
	guix system build -L $(MODULES) dotfiles/guixsd/vm1.wugi.info.scm

.PHONY: vm2.wugi.info
vm2.wugi.info:
	guix system build -L $(MODULES) dotfiles/guixsd/vm2.wugi.info.scm

.PHONY: vm3.wugi.info
vm3.wugi.info:
	guix system build -L $(MODULES) dotfiles/guixsd/vm3.wugi.info.scm

.PHONY: vm4.wugi.info
vm4.wugi.info:
	guix system build -L $(MODULES) dotfiles/guixsd/vm4.wugi.info.scm

.PHONY: vm5.wugi.info
vm5.wugi.info:
	guix system build -L $(MODULES) dotfiles/guixsd/vm5.wugi.info.scm

.PHONY: add
add:
	chezmoi add --encrypt $(HOME)/.ssh/known_hosts
	chezmoi add --encrypt $(HOME)/.ssh/authorized_keys
