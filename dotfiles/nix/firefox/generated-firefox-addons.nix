{ buildFirefoxXpiAddon, fetchurl, lib, stdenv }:
  {
    "highlightall" = buildFirefoxXpiAddon {
      pname = "highlightall";
      version = "2.3";
      addonId = "{26FD1F83-A45B-4c74-AF5A-F2EE0EE4D691}";
      url = "https://addons.mozilla.org/firefox/downloads/file/3684443/highlightall-2.3.xpi";
      sha256 = "9830f3b19ea267d9cb7a8c5dda9d0a663c2fb8eccb9d0752f527df322c6eccbd";
      meta = with lib;
      {
        homepage = "http://jgoudey.free.fr/highlightall/";
        description = "Automatically highlight and count all occurrences of a word by selecting it.\nCustom mode option provides multiple highlighting feature with customizable colors and can also display search markers beside the scrollbar.";
        license = licenses.gpl3;
        mozPermissions = [
          "<all_urls>"
          "notifications"
          "find"
          "storage"
          "contextMenus"
          ];
        platforms = platforms.all;
        };
      };
    }