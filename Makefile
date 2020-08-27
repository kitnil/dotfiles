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
	$(HOME)/src/guix-master/pre-inst-env \
	env GUIX_PACKAGE_PATH=$(HOME)/src/guix-wigust/guix \
	guix system vm --no-offload dotfiles/system/vm-image-stumpwm.tmpl

.PHONY: configure
configure:
	./configure

.PHONY: install
install:
	bin/executable_gpg-unlock
	chezmoi apply
	update-desktop-database ~/.local/share/applications
	open-with-linux install
	ln -sf $(HOME)/.nix-profile/share/mpv/scripts/notify-send.lua $(HOME)/.config/mpv/scripts/notify-send.lua
	dotfiles/idesk/idesk.scm
	install --mode=644 dotfiles/idesk/xterm.lnk $(HOME)/.idesktop
	install --mode=644 dotfiles/idesk/guix.vm.wugi.info.lnk $(HOME)/.idesktop
	install --mode=755 dotfiles/scripts/guix-channels-update $(HOME)/bin
	install --mode=755 dotfiles/scripts/guix-ci $(HOME)/bin
	install --mode=755 dotfiles/scripts/guix-package-version $(HOME)/bin
	install --mode=755 dotfiles/scripts/guix-profile-to-manifest $(HOME)/bin
	install --mode=755 dotfiles/scripts/maintenance $(HOME)/bin
	install --mode=755 dotfiles/scripts/rofi-mycli $(HOME)/bin
	install --mode=755 dotfiles/scripts/sshrc $(HOME)/bin
