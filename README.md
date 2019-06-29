Данный репозиторий предоставляет [Guix
channel](https://www.gnu.org/software/guix/manual/en/html_node/Channels.html),
который позволяет установить guile-ihs -
[Guile](https://www.gnu.org/software/guile/) клиент для HMS и billing2.

## Установка

Прежде всего необходимо установить пакетный менеджер [Guix](https://www.gnu.org/software/guix/).  Это можно
сделать с помощью официального скрипта, перед запуском которого
необходимо импортировать GPG ключ:
``` shell
gpg --keyserver pool.sks-keyservers.net --recv-keys 3CE464558A84FDC69DB40CFB090B11993D9AEBB5
wget -O guix-install.sh https://git.savannah.gnu.org/cgit/guix.git/plain/etc/guix-install.sh?h=v1.0.0
```

Запускать скрипт следует от root пользователя, например так:
``` shell
sudo -i ./guix-install.sh
```

Во время установки стоит согласится на использование substitute сервера, чтобы
не компилировать все из исходников на своем компьютере.

После установки добавить в свой создать файл `~/.config/guix/channels.scm`
(при отсутствии создать директорию `~/.config/guix/`):
``` scheme
(cons
 (channel
  (name 'guix-majordomo)
  (url "https://gitlab-ci-token:uhd8qabRUz5dD-HQchLi@gitlab.wugi.info/guix/guix-majordomo.git"))
 %default-channels)
```

Далее обновляем Guix package collection и устанавливаем guile-ihs:
``` shell
guix pull

guix install guile-ihs
```
