<https://nixos.wiki/wiki/Fonts>

> Flatpak applications can't find system fonts
>
> Enable fontDir in your NixOS configuration:
>
> fonts.fontDir.enable = true;
>
> Then, create a symlink in XDG_DATA_HOME/fonts pointing to /run/current-system/sw/share/X11/fonts, e. g.
>
> $ ln -s /run/current-system/sw/share/X11/fonts ~/.local/share/fonts

Also same symlink step is required for Steam.
