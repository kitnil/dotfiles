#!/bin/sh

cat <<EOF | xmenu | sh &
Applications
	Browsers
		IMG:/home/oleg/.nix-profile/share/icons/hicolor/32x32/apps/firefox.png	Firefox	firefox -P default
		IMG:/home/oleg/.nix-profile/share/icons/hicolor/24x24/apps/chromium.png	Chromium	chromium
		IMG:/home/oleg/.nix-profile/share/icons/hicolor/32x32/apps/firefox.png	Firefox (ESR-52)	firefox-esr-52 -P esr52 --new-instance
	Mail
		IMG:/home/oleg/.guix-profile/share/icons/gnome/24x24/emblems/emblem-mail.png	View	echo '(gnus-new-window)' | stumpish -e eval
		IMG:/home/oleg/.guix-profile/share/icons/gnome/24x24/actions/mail-send-receive.png	Pull	echo '(notmuch)' | stumpish -e eval
	IMG:/home/oleg/.guix-profile/share/icons/hicolor/24x24/apps/leafpad.png	Editor (leafpad)	leafpad
	IMG:/home/oleg/.guix-profile/share/icons/hicolor/24x24/apps/gimp.png	Image editor (Gimp)	gimp
	IMG:/home/oleg/.nix-profile/share/icons/robomongo.png	Mongo	robo3t
	IMG:/home/oleg/.guix-profile/share/icons/gnome/24x24/devices/video-display.png	OBS	record
	IMG:/home/oleg/.nix-profile/share/icons/hicolor/32x32/apps/quassel.png	Quassel	quassel
Container
	IMG:/home/oleg/.guix-profile/share/icons/hicolor/24x24/apps/qemu.png	Debian 10	echo '(docker-debian)' | stumpish -e eval
	IMG:/home/oleg/.guix-profile/share/icons/hicolor/24x24/apps/qemu.png	Common Lisp IDE (lem)	echo '(docker-lem)' | stumpish -e eval
	IMG:/home/oleg/.nix-profile/share/icons/hicolor/32x32/apps/firefox.png	Firefox (ESR-52)	echo '(docker-firefox-esr-52)' | stumpish -e eval
Virtual machine
	IMG:/home/oleg/.guix-profile/share/icons/hicolor/24x24/apps/qemu.png	Ubuntu 20.04	ubuntu
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
Wallpaper
	StumpWM Refcard	echo
	Planet	echo '(xplanet-file)' | stumpish -e eval
	Images	echo '(wallpaper)' | stumpish -e eval
Web
	Sketch	chromium --app=https://excalidraw.com/
EOF
