stow:
	stow --target=$(HOME) --dir=fiore natsu

update-mime:
	update-desktop-database ~/.local/share/applications/

all: stow update-mime
