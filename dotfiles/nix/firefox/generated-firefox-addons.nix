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
      version = "3.1.0";
      addonId = "prometheus-formatter@frederic-hemberger.de";
      url = "https://addons.mozilla.org/firefox/downloads/file/4166150/prometheus_formatter-3.1.0.xpi";
      sha256 = "2796e4a48ee6f813859ea252fe50dd0ab895bb570da4cb3caa3702d2f3413b22";
      meta = with lib;
      {
        homepage = "https://github.com/fhemberger/prometheus-formatter";
        description = "Makes plain Prometheus/OpenMetrics endpoints easier to read.";
        license = licenses.mit;
        mozPermissions = [ "storage" "http://*/*" "https://*/*" ];
        platforms = platforms.all;
        };
      };
    "scroll_anywhere" = buildFirefoxXpiAddon {
      pname = "scroll_anywhere";
      version = "9.2";
      addonId = "juraj.masiar@gmail.com_ScrollAnywhere";
      url = "https://addons.mozilla.org/firefox/downloads/file/3938344/scroll_anywhere-9.2.xpi";
      sha256 = "614a7a13baad91a4015347ede83b66ae3e182679932cfc4ccd8fa5604ab38e91";
      meta = with lib;
      {
        homepage = "https://fastaddons.com/";
        description = "Scroll page without touching scroll-bar! \nPress Middle (Right / Left) mouse button anywhere on the page to scroll just like with scrollbar.\n\nFeatures also:\n- \"grab and drag\" scrolling\n- customizable scrollbars!\n- the Momentum auto-scroll";
        mozPermissions = [ "alarms" "storage" "activeTab" "<all_urls>" ];
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
    "ultrawidify" = buildFirefoxXpiAddon {
      pname = "ultrawidify";
      version = "5.1.6";
      addonId = "{cf02b1a7-a01a-4e37-a609-516a283f1ed3}";
      url = "https://addons.mozilla.org/firefox/downloads/file/4060162/ultrawidify-5.1.6.xpi";
      sha256 = "57d26809e38af6612e2666ad50512329a94f4ece18dec57077be80f076c24437";
      meta = with lib;
      {
        homepage = "https://github.com/xternal7/ultrawidify";
        description = "Aspect ratio fixer for Youtube. Allows improperly encoded videos to properly fit ultrawide and superwide displays (e.g. 21:9) — and it attempts to do so automatically.";
        mozPermissions = [ "storage" "activeTab" "<all_urls>" "*://*/*" ];
        platforms = platforms.all;
        };
      };
    "visited-link-enabler" = buildFirefoxXpiAddon {
      pname = "visited-link-enabler";
      version = "0.3.0";
      addonId = "jid1-yDnsmkBoiRtgNA@jetpack";
      url = "https://addons.mozilla.org/firefox/downloads/file/803426/visited_link_enabler-0.3.0.xpi";
      sha256 = "7b1ab99b45c6015e48af73b27b937f32d32fb3ea32a9dac4d99bc6dc0133fa9f";
      meta = with lib;
      {
        description = "enables visited link colors on almost every website for easier browsing";
        license = licenses.mpl20;
        mozPermissions = [
          "<all_urls>"
          "storage"
          "activeTab"
          "tabs"
          "contextMenus"
          ];
        platforms = platforms.all;
        };
      };
    }