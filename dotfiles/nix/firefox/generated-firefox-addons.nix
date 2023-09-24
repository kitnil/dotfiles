{ buildFirefoxXpiAddon, fetchurl, lib, stdenv }:
  {
    "access-control-allow-origin" = buildFirefoxXpiAddon {
      pname = "access-control-allow-origin";
      version = "0.1.7";
      addonId = "{c5f935cf-9b17-4b85-bed8-9277861b4116}";
      url = "https://addons.mozilla.org/firefox/downloads/file/3974756/access_control_allow_origin-0.1.7.xpi";
      sha256 = "eb600f22afdb4834acb2032bf685bbb6449daef2e3c725765d9d22935e329bf3";
      meta = with lib;
      {
        homepage = "https://mybrowseraddon.com/access-control-allow-origin.html";
        description = "Easily add (Access-Control-Allow-Origin: *) rule to the response header.";
        license = licenses.mpl20;
        mozPermissions = [
          "storage"
          "activeTab"
          "<all_urls>"
          "webRequest"
          "webRequestBlocking"
          ];
        platforms = platforms.all;
        };
      };
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
    "prometheus-formatter" = buildFirefoxXpiAddon {
      pname = "prometheus-formatter";
      version = "2.2.0";
      addonId = "prometheus-formatter@frederic-hemberger.de";
      url = "https://addons.mozilla.org/firefox/downloads/file/3785090/prometheus_formatter-2.2.0.xpi";
      sha256 = "0wbg8x2w3qf5id9fbi8kh809zyppc0srnf7ggw6vkhdax0gn58p9";
      meta = with lib;
      {
        homepage = "https://github.com/fhemberger/prometheus-formatter";
        description = "Makes plain Prometheus/OpenMetrics endpoints easier to read.";
        license = licenses.mit;
        mozPermissions = [ "storage" "http://*/*" "https://*/*" ];
        platforms = platforms.all;
        };
      };
    "snaplinksplus" = buildFirefoxXpiAddon {
      pname = "snaplinksplus";
      version = "3.1.11";
      addonId = "snaplinks@snaplinks.mozdev.org";
      url = "https://addons.mozilla.org/firefox/downloads/file/3838174/snaplinksplus-3.1.11.xpi";
      sha256 = "5e7077ae5619d7da64f48af193d965f997db93e9c694ac38e3baf6854d25aa19";
      meta = with lib;
      {
        homepage = "http://cpriest.github.io/SnapLinksPlus/";
        description = "Select a number of links with a rectangle and open them in new tabs.  You can also lasso checkboxes to quickly check or uncheck them.  Works with radio buttons as well.";
        license = licenses.mit;
        mozPermissions = [
          "cookies"
          "storage"
          "notifications"
          "clipboardWrite"
          "<all_urls>"
          ];
        platforms = platforms.all;
        };
      };
    }