## Установка через Nix

``` shell
git -c http.sslVerify=false clone https://gitlab.intr/utils/nix-docker-firefox-esr
NIXPKGS_CONFIG="$PWD/config.nix" NIX_PATH="nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz" nix-env -iA nixpkgs.firefox-esr-52
```

## Установка через Docker

Установить сертификат:
``` shell
bash ./Majordomo_LLC_Root_CA.crt.sh
```

Склонировать контейнер:
``` shell
sudo docker pull docker-registry.intr/utils/nix-docker-firefox-esr:master
```

Посмотреть команду запуска контейнера:
``` shell
sudo docker inspect docker-registry.intr/utils/nix-docker-firefox-esr:master | grep cmd
```

Скопировать команду запуска контейнера в свой shell, например:
``` shell
xhost +local:; sudo docker run --network=host --tty --interactive --rm --volume /etc/localtime:/etc/localtime:ro --volume /tmp/.X11-unix:/tmp/.X11-unix --user 1000:997 --env DISPLAY=$DISPLAY docker-registry.intr/utils/nix-docker-firefox-esr:master
```
