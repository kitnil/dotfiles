build-systems:
	guix system build -L "fiore/modules" guixsd/guixsd.scm
	guix system build -L "fiore/modules" guixsd/spb.scm
	guix system build -L "fiore/modules" guixsd/workstation.scm

deploy: build-systems
	guix deploy -L fiore/modules --no-build-hook guixsd/deploy.scm

stow:
	stow --target=$(HOME) --dir=$(PWD) oleg

update-mime:
	update-desktop-database ~/.local/share/applications/

update-channels:
	guile/guix-channels-update

all: stow update-mime
