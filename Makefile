pull:
	xpanes -C 1 -c 'ssh -t {} -- sh -c "guix pull --url=https://cgit.duckdns.org/git/guix/guix && sudo -i guix pull --url=https://cgit.duckdns.org/git/guix/guix; read -n 1 -s -r -p \"Press any key to close.\""' guixsd workstation spb

build-systems:
	guix system build -L "fiore/modules" guixsd/guixsd.scm
	guix system build -L "fiore/modules" guixsd/spb.scm
	guix system build -L "fiore/modules" guixsd/workstation.scm

reconfigure-guixsd: guixsd/guixsd.scm
	sudo -i guix system reconfigure --load-path="$(HOME)/src/dotfiles/fiore/modules" --substitute-urls='https://ci.guix.info http://cuirass.tld' --fallback "$(HOME)/src/dotfiles/guixsd/guixsd.scm"

deploy: build-systems
	guix deploy -L fiore/modules --no-build-hook guixsd/deploy.scm

stow:
	stow --target=$(HOME) --dir=$(PWD) oleg

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

tail-cuirass-pull:
	$(HOME)/.nix-profile/bin/ansible-playbook $(HOME)/src/hello-ansible/git.yml

update-channels:
	guile/guix-channels-update

all: stow update-mime
