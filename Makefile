TESTS =						\
  tests/connect.bats				\
  tests/executables.bats			\
  tests/mail.bats				\
  tests/mjru.bats				\
  tests/guix.bats

.PHONY: clean
clean:
	rm -rf test-tmp

.PHONY: check
check:
	bats $(TESTS)

.PHONY: check-system
check-system:
	guix system vm -L $(HOME)/.local/share/chezmoi/dotfiles/guixsd/modules --no-offload dotfiles/system/vm-image-stumpwm.tmpl

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
	sudo install dotfiles/homer/config.yml /etc/homer
	gpg --decrypt dotfiles/emacs/mjru-network.gpg > $(HOME)/.emacs.d/modules/mjru-network.el

.PHONY: deploy
deploy:
	guix deploy -L $(HOME)/.local/share/chezmoi/dotfiles/guixsd/modules dotfiles/guixsd/deploy.scm

.PHONY: dist
dist:
	sudo --login guix system build -L $(HOME)/.local/share/chezmoi/dotfiles/guixsd/modules $(HOME)/.local/share/chezmoi/dotfiles/guixsd/guixsd.scm
	sudo --login guix system build -L $(HOME)/.local/share/chezmoi/dotfiles/guixsd/modules $(HOME)/.local/share/chezmoi/dotfiles/guixsd/workstation-guixsd.scm
	sudo --login guix system build -L $(HOME)/.local/share/chezmoi/dotfiles/guixsd/modules $(HOME)/.local/share/chezmoi/dotfiles/guixsd/spb.scm
	sudo --login guix system build -L $(HOME)/.local/share/chezmoi/dotfiles/guixsd/modules $(HOME)/.local/share/chezmoi/dotfiles/guixsd/guix.vm.wugi.info.scm
