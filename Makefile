TESTS =						\
  tests/connect.bats				\
  tests/executables.bats			\
  tests/mail.bats				\
  tests/mjru.bats				\
  tests/guix.bats				\
  tests/ssh-mjru.bats				\
  tests/ssh-home.bats

.PHONY: clean
clean:
	rm -rf test-tmp
	rm -f dotfiles/nix/result

.PHONY: check
check:
	mkdir test-tmp
	gpg --quiet --decrypt dhall/ssh/ssh.dhall.gpg | dhall text > test-tmp/mjru.conf
	gpg --quiet --decrypt private_dot_ssh/encrypted_private_config > test-tmp/config
	gpg --quiet --decrypt private_dot_ssh/encrypted_private_spb.conf > test-tmp/spb.conf
	bats $(TESTS)

.PHONY: vm
vm:
	$(shell guix system vm -L $(HOME)/.local/share/chezmoi/dotfiles/guixsd/modules --no-offload dotfiles/system/vm-image-stumpwm.tmpl) -nic user,model=virtio-net-pci,hostfwd=tcp::10022-:22

.PHONY: extension-graph
extension-graph:
	guix system -L $(HOME)/.local/share/chezmoi/dotfiles/guixsd/modules extension-graph dotfiles/guixsd/guixsd.scm | xdot -

.PHONY: shepherd-graph
shepherd-graph:
	guix system -L $(HOME)/.local/share/chezmoi/dotfiles/guixsd/modules shepherd-graph dotfiles/guixsd/guixsd.scm | xdot -

.PHONY: configure
configure:
	./configure

.PHONY: install
install:
	bin/executable_gpg-unlock
	chezmoi apply
	update-desktop-database ~/.local/share/applications
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
	gpg --quiet --decrypt dhall/ssh/ssh.dhall.gpg | dhall text > $(HOME)/.ssh/mjru.conf

.PHONY: deploy
deploy:
	guix deploy -L $(HOME)/.local/share/chezmoi/dotfiles/guixsd/modules dotfiles/guixsd/deploy.scm

.PHONY: dist
dist:
	sudo --login guix system build -L $(HOME)/.local/share/chezmoi/dotfiles/guixsd/modules $(HOME)/.local/share/chezmoi/dotfiles/guixsd/guixsd.scm
	sudo --login guix system build -L $(HOME)/.local/share/chezmoi/dotfiles/guixsd/modules $(HOME)/.local/share/chezmoi/dotfiles/guixsd/ws1.wugi.info.scm
	sudo --login guix system build -L $(HOME)/.local/share/chezmoi/dotfiles/guixsd/modules $(HOME)/.local/share/chezmoi/dotfiles/guixsd/spb.scm
	sudo --login guix system build -L $(HOME)/.local/share/chezmoi/dotfiles/guixsd/modules $(HOME)/.local/share/chezmoi/dotfiles/guixsd/guix.vm.wugi.info.scm

.PHONY: add
add:
	chezmoi add --encrypt $(HOME)/.ssh/known_hosts
