This repository contains my configuration files.

To setup them I use [GNU Stow](https://www.gnu.org/software/stow/) and
[GNU Make](https://www.gnu.org/software/make/):

``` {.example}
$ make stow
```

Some files could conflict. Stow will tell which are. So make sure
to backup them moving somewhere with:

``` {.example}
$ mv CONFLICT_CONFIG_FILE BACKUP_DIRECTORY
```

Make sure update `mimeapps.list`:

``` {.example}
$ update-desktop-database ~/.local/share/applications/
```

Packages are managed by [GNU Guix](https://guix.gnu.org/):

``` {.shell}
guix package --substitute-urls='https://ci.guix.info \
     --manifest=manifests/oleg.scm
```

Thanks:

- [Alex Kost StumpWM configs](https://github.com/wiedzmin/stumpwm-config)
