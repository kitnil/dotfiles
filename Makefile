stow:
	stow --target=$(HOME) --dir=fiore natsu

update-mime:
	update-desktop-database ~/.local/share/applications/

clover-build:
	env GUIX_PACKAGE_PATH=$(HOME)/src/guix-packages:$(GUIX_PACKAGE_PATH) guix-system-build /home/natsu/dotfiles/fiore/clover.scm

clover-reconfigure:
	env GUILE_LOAD_PATH=$(HOME)/src/guix-wigust:$(HOME)/src/guix-packages:$(GUILE_LOAD_PATH) sudo -E guix system reconfigure /home/natsu/dotfiles/fiore/clover.scm

magnolia-build-dry:
	env GUILE_LOAD_PATH=$(HOME)/dotfiles:$(GUILE_LOAD_PATH) guix system build --dry-run $(HOME)/dotfiles/fiore/magnolia.scm

magnolia-build:
	env GUILE_LOAD_PATH=$(HOME)/dotfiles:$(GUILE_LOAD_PATH) guix system build $(HOME)/dotfiles/fiore/magnolia.scm

magnolia-build-pre:
	$(HOME)/src/guix/pre-inst-env env GUILE_LOAD_PATH=$(HOME)/.config/guix/current/share/guile/site/2.2:$(HOME)/dotfiles:$(GUILE_LOAD_PATH) guix system build $(HOME)/dotfiles/fiore/magnolia.scm

magnolia-reconfigure:
	sudo env HOME=/home/natsu GUILE_LOAD_PATH=$(HOME)/dotfiles:$(GUILE_LOAD_PATH) guix system reconfigure $(HOME)/dotfiles/fiore/magnolia.scm

all: stow update-mime
