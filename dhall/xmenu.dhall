-- -*- eval: (setq-local indent-tabs-mode t); -*-

let config = "saint-petersburg"

let guix-profile = "/home/oleg/.guix-profile"

let nix-profile = "/home/oleg/.nix-profile"

let project =
		λ(args : { name : Text, session : Text })
	  → "\t${args.name}\txterm -sl 4096 +sb -bg black -fg white -e 'tmuxifier load-session ${args.session}'"

let container =
		λ(args : { name : Text, icon : Text, command : Text })
	  → "\tIMG:${args.icon}\t${args.name}\t${args.command}"

let hms-account =
		λ(args : { name : Text, server : Text, tarif : Text })
	  → "${args.server} ${args.name} ${args.tarif}\n\t\t\topen\t\t\t\thms web open ${args.name}\n\t\t\ttoggle\t\t\t\thms web toggle ${args.name}"

let service = λ(args : { name : Text, start : Text, stop : Text })
	  → "${args.name}\n\t\tStart\t${args.start}\n\t\tStop\t${args.stop}"

in  ''
	#!/bin/sh

	cat <<EOF | xmenu | sh &
	Applications
		Audio
			Pavucontrol	sh -c "echo '(pavucontrol)' | stumpish -e eval"
		Browsers
			IMG:${nix-profile}/share/icons/hicolor/32x32/apps/firefox.png	Firefox	firefox -P default
			IMG:${nix-profile}/share/icons/hicolor/24x24/apps/chromium.png	Chromium	echo '(chromium)' | stumpish -e eval
			IMG:${nix-profile}/share/icons/hicolor/32x32/apps/firefox.png	Firefox (ESR-52)	firefox-esr-52 -P esr52 --new-instance
		Databases
			IMG:${nix-profile}/share/icons/robomongo.png	Mongo	robo3t
		Mail
			IMG:${guix-profile}/share/icons/gnome/24x24/emblems/emblem-mail.png	View	echo '(gnus-new-window)' | stumpish -e eval
			IMG:${guix-profile}/share/icons/gnome/24x24/actions/mail-send-receive.png	Pull	echo '(notmuch)' | stumpish -e eval
		Editor
			Emacs (Emacs Daemon)	run-emacs
			IMG:${guix-profile}/share/icons/hicolor/24x24/apps/leafpad.png	Leafpad	leafpad
		Guix
			Update	st -f Monospace:size=12 bash -ic 'guix menu; dialog --yesno "Press any key to close." 0 0'
		Games
			Eve Online	eve-online
			Tales of Maj'Eyal	env DRI_PRIME=1 tome4
		Images
			Mypaint	env PYTHONPATH= mypaint
			IMG:${guix-profile}/share/icons/hicolor/24x24/apps/gimp.png	Gimp	gimp
		IMG:${nix-profile}/share/icons/hicolor/32x32/apps/quassel.png	Quassel	"echo '(quassel)' | stumpish -e eval"
		IMG:${guix-profile}/share/icons/gnome/24x24/devices/video-display.png	OBS	env DRI_PRIME=1 LIBVA_DRIVER_NAME=radeonsi obs
		Screen
			Screenshot	xfce4-screenshooter
			Zoom screen	echo '(zoom)' | stumpish -e eval
		Terminal
			XTerm	xterm -sl 4096 +sb -bg black -fg white
			st	st -f Monospace:size=12
			Kitty	kitty
			XFCE	xfce4-terminal
			Termonad	termonad
			QTerminal	echo '(qterminal)' | stumpish -e eval

	Services
		${service { name = "Espanso", start = "espanso start", stop = "espanso stop" }}
		${service { name = "xmodmap", start = "xmodmap /home/oleg/.Xmodmap", stop = "" }}
		${service { name = "webhook", start = "echo '(webhook)' | stumpish -e eval", stop = "" }}
		${service { name = "vnc-5901", start = "vnc server 5901", stop = "vnc-server -kill :5901" }}
		${service { name = "vnc-5902", start = "vnc server 5902", stop = "vnc-server -kill :5902" }}
		${service { name = "vnc-5903", start = "vnc server 5903", stop = "vnc-server -kill :5903" }}

	Projects
	${project { name = "Backup", session = "backup" }}
	${project { name = "Blog", session = "blog" }}
	${project { name = "Dotfiles", session = "dotfiles" }}
	${project { name = "Guix", session = "guix" }}
	${project { name = "NixOps", session = "nixops" }}

	Documention
		Arch Wiki	firefox file:///home/oleg/.local/share/arch-wiki/html
		Zeal	echo '(zeal)' | stumpish -e eval
		Books
			Linux System Programming	zathura /home/oleg/Downloads/beginning-linux-programming/beginning-linux-programming.pdf

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
		IMG:${guix-profile}/share/icons/hicolor/24x24/apps/qemu.png	Windows 10 (disk)	sudo qemu-windows10

	Wallpaper
		StumpWM Refcard	echo '(refcard-stumpwm)' | stumpish -e eval
		Planet	echo '(xplanet-file)' | stumpish -e eval
		Images	echo '(wallpaper)' | stumpish -e eval
		Paperview	paperview

	Work
		IPMIView	ipmiview
		Sampler	echo '(sampler)' | stumpish -e eval
		Shared
			${hms-account { server = "web15", name = "AC_222914", tarif = "Хостинг приложений" }}
			${hms-account { server = "web15", name = "AC_221357", tarif = "Старт" }}
			${hms-account { server = "web15", name = "AC_230157", tarif = "Безлимитный" }}
			${hms-account { server = "web16", name = "AC_228919", tarif = "Безлимитный+" }}
			${hms-account { server = "web19", name = "AC_224973", tarif = "Безлимитный+" }}
			${hms-account { server = "web20", name = "AC_226927", tarif = "Безлимитный+" }}
			${hms-account { server = "web24", name = "AC_227061", tarif = "Безлимитный+" }}
			${hms-account { server = "web26", name = "AC_220037", tarif = "Безлимитный+" }}
			${hms-account { server = "web26", name = "AC_228131", tarif = "Безлимитный" }}
			${hms-account { server = "web27", name = "AC_220783", tarif = "Безлимитный" }}
			${hms-account { server = "web29", name = "AC_216226", tarif = "Безлимитный" }}
			${hms-account { server = "web30", name = "AC_226391", tarif = "Безлимитный" }}
			${hms-account { server = "web30", name = "AC_220670", tarif = "Безлимитный+" }}
			${hms-account { server = "web30", name = "AC_200388", tarif = "Безлимитный" }}
			${hms-account { server = "web32", name = "AC_227596", tarif = "Безлимитный" }}
			${hms-account { server = "web33", name = "AC_208112", tarif = "Безлимитный" }}
			${hms-account { server = "web34", name = "AC_224079", tarif = "Безлимитный" }}
			${hms-account { server = "web37", name = "AC_213062", tarif = "Безлимитный" }}
			${hms-account { server = "web15", name = "AC_222228", tarif = "Корпоративный" }}
			${hms-account { server = "kvm-test", name = "AC_223473", tarif = "Безлимитный" }}
			${hms-account { server = "majordomo", name = "AC_222228", tarif = "Бизнес+" }}
			${hms-account { server = "web23", name = "AC_189986", tarif = "Служебный" }}
		VPS
			Open my account	firefox https://billing2.intr/client/vds/17959

	Web
		Algorithms	chromium --app=https://algorithm-visualizer.org/
		Byte converter	chromium --app=https://whatsabyte.com/P1/byteconverter.htm
		Chess	chromium --app=https://lichess.org/
		Cocalc	chromium --app=https://cocalc.com/
		Diagrams	chromium	--app=https://app.diagrams.net/
		LaTeX	chromium --app=http://www.overleaf.com/
		Habitica	chromium --app=https://habitica.com/
		HTTP Requests	chromium --app=https://www.hurlit.com/
		Mastodon	chromium --app=https://mastodon.xyz/
		Repl	chromium --app=https://repl.it/
		Sketch	chromium --app=https://excalidraw.com/
		Spotify	chromium --app=https://open.spotify.com
		The Try It Online	chromium --app=https://tio.run/
		Weather	chromium --app=https://yandex.ru/pogoda/
	EOF
	''
