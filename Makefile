stow:
	stow --target=$(HOME) --dir=fiore natsu

update-mime:
	update-desktop-database ~/.local/share/applications/

clover-build:
	env GUIX_PACKAGE_PATH=$(HOME)/src/guix-packages:$(GUIX_PACKAGE_PATH) guix-system-build /home/natsu/dotfiles/fiore/clover.scm

clover-reconfigure:
	env GUILE_LOAD_PATH=$(HOME)/src/guix-wigust:$(HOME)/src/guix-packages:$(GUILE_LOAD_PATH) sudo -E guix system reconfigure /home/natsu/dotfiles/fiore/clover.scm

magnolia-build:
	env GUIX_PACKAGE_PATH=$(GUIX_PACKAGE_PATH):$(HOME)/dotfiles GUILE_LOAD_PATH=$(HOME)/src/guix-wigust:$(HOME)/dotfiles:$(GUILE_LOAD_PATH) guix system build $(HOME)/dotfiles/fiore/magnolia.scm

magnolia-reconfigure:
	sudo env HOME=/home/natsu GUIX_PACKAGE_PATH=$(HOME)/dotfiles GUILE_LOAD_PATH=$(HOME)/src/guix-wigust:$(GUILE_LOAD_PATH) guix system reconfigure $(HOME)/dotfiles/fiore/magnolia.scm

all: stow update-mime
