-- -*- eval: (setq-local indent-tabs-mode t); -*-

let config = /home/oleg/.config/dhall/config.dhall

let guix-profile = "/home/oleg/.guix-profile"

let nix-profile = "/home/oleg/.nix-profile"

let project =
		λ(args : { name : Text, session : Text })
	  → "\t${args.name}\txterm -sl 4096 +sb -bg black -fg white -e 'tmuxifier load-session ${args.session}'"

let container =
		λ(args : { name : Text, icon : Text, command : Text })
	  → "\tIMG:${args.icon}\t${args.name}\t${args.command}"

in  ''
	#!/bin/sh

	cat <<EOF | xmenu | sh &
	Applications
		Browsers
			IMG:${nix-profile}/share/icons/hicolor/32x32/apps/firefox.png	Firefox	firefox -P default
			IMG:${nix-profile}/share/icons/hicolor/24x24/apps/chromium.png	Chromium	echo '(chromium)' | stumpish -e eval
			IMG:${nix-profile}/share/icons/hicolor/32x32/apps/firefox.png	Firefox (ESR-52)	firefox-esr-52 -P esr52 --new-instance
		Mail
			IMG:${guix-profile}/share/icons/gnome/24x24/emblems/emblem-mail.png	View	echo '(gnus-new-window)' | stumpish -e eval
			IMG:${guix-profile}/share/icons/gnome/24x24/actions/mail-send-receive.png	Pull	echo '(notmuch)' | stumpish -e eval
		Espanso
			Start	espanso start
			Stop	espanso stop
		Emacs (Emacs Daemon)	run-emacs
		Documention (Zeal)	echo '(zeal)' | stumpish -e eval
		IMG:${guix-profile}/share/icons/hicolor/24x24/apps/leafpad.png	Editor (leafpad)	leafpad
		IMG:${guix-profile}/share/icons/hicolor/24x24/apps/gimp.png	Image editor (Gimp)	gimp
		IMG:${nix-profile}/share/icons/robomongo.png	Mongo	robo3t
		IMG:${guix-profile}/share/icons/gnome/24x24/devices/video-display.png	OBS	echo '(obs)' | stumpish -e eval
		IMG:${nix-profile}/share/icons/hicolor/32x32/apps/quassel.png	Quassel	"echo '(quassel)' | stumpish -e eval"

	Projects
	${project { name = "Blog", session = "blog" }}
	${project { name = "Dotfiles", session = "dotfiles" }}
	${project { name = "Guix", session = "guix" }}
	${project { name = "NixOps", session = "nixops" }}

	Guix
		Update	xterm +sb -bg black -fg white -e 'bash -ic "guix menu"; read -p "Press any key to close."'

	Container
	${container
		{ icon = "${guix-profile}/share/icons/hicolor/24x24/apps/qemu.png"
		, name = "Debian 10"
		, command = "echo '(docker-debian)' | stumpish -e eval"
		}}
	${container
		{ icon = "${guix-profile}/share/icons/hicolor/24x24/apps/qemu.png"
		, name = "Common Lisp IDE (lem)"
		, command = "echo '(docker-lem)' | stumpish -e eval"
		}}
	${container
		{ icon = "${nix-profile}/share/icons/hicolor/32x32/apps/firefox.png"
		, name = "Firefox (ESR-52)"
		, command = "echo '(docker-firefox-esr-52)' | stumpish -e eval"
		}}

	Virtual machine
		IMG:${guix-profile}/share/icons/hicolor/24x24/apps/qemu.png	Ubuntu 20.04	ubuntu

	StumpWM
		Split
			Horizontally	echo '(hsplit)' | stumpish -e eval
			Vertically	echo '(vsplit)' | stumpish -e eval

	Terminal
		XTerm	xterm -sl 4096 +sb -bg black -fg white
		st	st -f Monospace:size=12
		Kitty	kitty
		XFCE	xfce4-terminal
		Termonad	termonad
		QTerminal	"echo '(qterminal)' | stumpish -e eval"

	Wallpaper
		StumpWM Refcard	echo
		Planet	echo '(xplanet-file)' | stumpish -e eval
		Images	echo '(wallpaper)' | stumpish -e eval
		Paperview	paperview

	Web
		Algorithms	chromium --app=https://algorithm-visualizer.org/
		Byte converter	chromium --app=https://whatsabyte.com/P1/byteconverter.htm
		Camera
			Home	camera home
			Street	camera street
		Chess	chromium --app=https://lichess.org/
		Cocalc	chromium --app=https://cocalc.com/
		Habitica	chromium --app=https://habitica.com/
		Mastodon	chromium --app=https://mastodon.xyz/
		Repl	chromium --app=https://repl.it/
		Sketch	chromium --app=https://excalidraw.com/
		Spotify	chromium --app=https://open.spotify.com
		The Try It Online	chromium --app=https://tio.run/
		Weather	chromium --app=https://yandex.ru/pogoda/
	EOF
	''
