# Secret files

- /etc/ddclient/secrets.conf
- /etc/guix
- /etc/ssh
- /etc/letsencrypt
- /etc/openvpn
- /etc/zabbix
- /etc/autossh/
- /etc/nix

# Nix

Nix packages uses /etc/fonts, which could be a requirement to run some
programs.

- sudo ln -s $HOME/.guix-profile/etc/fonts /etc/fonts
