TESTS =						\
  tests/connect.bats				\
  tests/executables.bats			\
  tests/mail.bats				\
  tests/majordomo.bats				\
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

.PHONY: install
install:
	bin/executable_gpg-unlock
	chezmoi apply
	update-desktop-database ~/.local/share/applications
	open-with-linux install
	mkdir -p $(HOME)/.config/mpv/scripts
	ln -sf $(HOME)/.nix-profile/share/mpv/scripts/notify-send.lua $(HOME)/.config/mpv/scripts/notify-send.lua
