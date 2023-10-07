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
    "auto_highlight" = buildFirefoxXpiAddon {
      pname = "auto_highlight";
      version = "5.1";
      addonId = "admin@fastaddons.com_AutoHighlight";
      url = "https://addons.mozilla.org/firefox/downloads/file/4130006/auto_highlight-5.1.xpi";
      sha256 = "377b4f4141c39a9c4ffd9ed66a7af60ac714d451e17661c64925a2c144ca24d9";
      meta = with lib;
      {
        homepage = "https://fastaddons.com/";
        description = "🟨 Auto Highlight will help you see what's important on the web! ✅\n⚡ Supports RegExp search and many style and highlight options ⭕";
        mozPermissions = [
          "contextMenus"
          "storage"
          "scripting"
          "alarms"
          "activeTab"
          "<all_urls>"
          ];
        platforms = platforms.all;
        };
      };
    "cookie-quick-manager" = buildFirefoxXpiAddon {
      pname = "cookie-quick-manager";
      version = "0.5rc2";
      addonId = "{60f82f00-9ad5-4de5-b31c-b16a47c51558}";
      url = "https://addons.mozilla.org/firefox/downloads/file/3343599/cookie_quick_manager-0.5rc2.xpi";
      sha256 = "b826e443438c880b3998e42e099d0e1949ff51489c788b50193b92ef80426c6e";
      meta = with lib;
      {
        homepage = "https://github.com/ysard/cookie-quick-manager";
        description = "An addon to manage cookies (view, search, create, edit, remove, backup, restore, protect from deletion and much more). Firefox 57+ is supported.";
        license = licenses.gpl3;
        mozPermissions = [
          "cookies"
          "<all_urls>"
          "activeTab"
          "storage"
          "browsingData"
          "contextualIdentities"
          "privacy"
          ];
        platforms = platforms.all;
        };
      };
    "copy-all-tab-urls-we" = buildFirefoxXpiAddon {
      pname = "copy-all-tab-urls-we";
      version = "2.2.0";
      addonId = "{0507f777-2480-4d48-baf1-3b9c8feeb2b4}";
      url = "https://addons.mozilla.org/firefox/downloads/file/3988710/copy_all_tab_urls_we-2.2.0.xpi";
      sha256 = "ac819a1fc0edb5293be0781a3308a99a70b64e9690a38b5a4feb71e53ac5d51c";
      meta = with lib;
      {
        description = "Copy All Tab Urls";
        license = licenses.mpl20;
        mozPermissions = [
          "tabs"
          "clipboardWrite"
          "storage"
          "notifications"
          "contextMenus"
          ];
        platforms = platforms.all;
        };
      };
    "copy-as-org-mode" = buildFirefoxXpiAddon {
      pname = "copy-as-org-mode";
      version = "0.2.0";
      addonId = "{59e590fc-6635-45fe-89c7-af637eb4b9c0}";
      url = "https://addons.mozilla.org/firefox/downloads/file/3939068/copy_as_org_mode-0.2.0.xpi";
      sha256 = "dcd02dbd1a753928b82e772055a0532421f94bb40ae23b0606e6e91117909cce";
      meta = with lib;
      {
        homepage = "https://github.com/kuanyui/copy-as-org-mode";
        description = "Copy selection or link of current page as Org-mode format text!";
        license = licenses.mpl20;
        mozPermissions = [
          "activeTab"
          "clipboardWrite"
          "menus"
          "storage"
          "notifications"
          ];
        platforms = platforms.all;
        };
      };
    "foxscroller" = buildFirefoxXpiAddon {
      pname = "foxscroller";
      version = "1.8";
      addonId = "{56b215f4-29b6-4898-bf2a-152d8bc189ed}";
      url = "https://addons.mozilla.org/firefox/downloads/file/3935307/foxscroller-1.8.xpi";
      sha256 = "76e3df60531c6d7ab947afffeb3c3148d5f8baa37dc6ccadfbea508867aa02e4";
      meta = with lib;
      {
        description = "Auto scroll web pages with fully adjustable speed.";
        license = licenses.gpl3;
        mozPermissions = [
          "activeTab"
          "contextMenus"
          "storage"
          "webNavigation"
          "<all_urls>"
          ];
        platforms = platforms.all;
        };
      };
    "google-container" = buildFirefoxXpiAddon {
      pname = "google-container";
      version = "1.5.4";
      addonId = "@contain-google";
      url = "https://addons.mozilla.org/firefox/downloads/file/3736912/google_container-1.5.4.xpi";
      sha256 = "47a7c0e85468332a0d949928d8b74376192cde4abaa14280002b3aca4ec814d0";
      meta = with lib;
      {
        homepage = "https://github.com/containers-everywhere/contain-google";
        description = "THIS IS NOT AN OFFICIAL ADDON FROM MOZILLA!\nIt is a fork of the Facebook Container addon.\n\nPrevent Google from tracking you around the web. The Google Container extension helps you take control and isolate your web activity from Google.";
        license = licenses.mpl20;
        mozPermissions = [
          "<all_urls>"
          "contextualIdentities"
          "cookies"
          "management"
          "tabs"
          "webRequestBlocking"
          "webRequest"
          "storage"
          ];
        platforms = platforms.all;
        };
      };
    "hello-goodbye" = buildFirefoxXpiAddon {
      pname = "hello-goodbye";
      version = "1.7.0";
      addonId = "{ddb52f85-5859-49b1-b54e-ee7709b1eb5d}";
      url = "https://addons.mozilla.org/firefox/downloads/file/4101286/hello_goodbye-1.7.0.xpi";
      sha256 = "6853a778fee464a77920ccfece2779be9f4d6eb465384daea05b543f6440091f";
      meta = with lib;
      {
        homepage = "https://hellogoodbye.app";
        description = "Hello, Goodbye blocks annoying chat widgets and cookie banners to make your internet a little bit better.";
        license = licenses.mit;
        mozPermissions = [
          "storage"
          "webRequest"
          "webRequestBlocking"
          "*://*/*"
          ];
        platforms = platforms.all;
        };
      };
    "metube-downloader" = buildFirefoxXpiAddon {
      pname = "metube-downloader";
      version = "1.3.4";
      addonId = "{6c6751df-7510-4e27-9637-9ae354c86f8c}";
      url = "https://addons.mozilla.org/firefox/downloads/file/4085631/metube_downloader-1.3.4.xpi";
      sha256 = "e8f5573d4b944a45068838ec5302be07c94e6f19ab1d7bd52ddadee58114d52d";
      meta = with lib;
      {
        description = "Queue download to your MeTube instance by context menu or top bar button.";
        license = licenses.gpl3;
        mozPermissions = [ "activeTab" "menus" "storage" ];
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
    "right-click-search" = buildFirefoxXpiAddon {
      pname = "right-click-search";
      version = "1.1.0";
      addonId = "{bbbb88d7-7da7-47e6-8836-d7d329e92dd9}";
      url = "https://addons.mozilla.org/firefox/downloads/file/3809250/right_click_search-1.1.0.xpi";
      sha256 = "952c1163fa4cae9cca96307cbd88e6a13d6b79fb68dd33d1c0530b4017a57b54";
      meta = with lib;
      {
        description = "Use the context menu to search using the engines in the Firefox search settings.\n\nThis add-on requires Firefox version 63 or later.";
        license = licenses.mit;
        mozPermissions = [ "search" "contextMenus" ];
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
    "tab-slideshow-we" = buildFirefoxXpiAddon {
      pname = "tab-slideshow-we";
      version = "0.7.1";
      addonId = "{ff4c3ef4-7337-4e7f-aa99-77ed911ef8b1}";
      url = "https://addons.mozilla.org/firefox/downloads/file/2983447/tab_slideshow_we-0.7.1.xpi";
      sha256 = "74788e3b88ba912f26304ef8b10b9bea46fafaaa044a4ff4cb1ff01ea33fb65a";
      meta = with lib;
      {
        description = "Slideshow at tabs.";
        license = licenses.mpl20;
        mozPermissions = [ "storage" ];
        platforms = platforms.all;
        };
      };
    "twitch-error-autorefresher" = buildFirefoxXpiAddon {
      pname = "twitch-error-autorefresher";
      version = "4";
      addonId = "{e7f57420-5fd4-4196-9a11-ad7fc8fc921d}";
      url = "https://addons.mozilla.org/firefox/downloads/file/4122986/twitch_autorefresher_error2000-4.xpi";
      sha256 = "cb08d1eec86d7a7b9e4bc285d82527e6b23142b25f86f58dddc13a08bd328f0d";
      meta = with lib;
      {
        description = "This extension will automatically refresh twitch stream whenever any error occurs.";
        license = licenses.gpl3;
        mozPermissions = [ "*://www.twitch.tv/*" ];
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
    "view-page-archive" = buildFirefoxXpiAddon {
      pname = "view-page-archive";
      version = "4.1.0";
      addonId = "{d07ccf11-c0cd-4938-a265-2a4d6ad01189}";
      url = "https://addons.mozilla.org/firefox/downloads/file/4139299/view_page_archive-4.1.0.xpi";
      sha256 = "8c5d42c863981044b999b4c10cbb7e87cc86da569e158707d70c4eec946d8edc";
      meta = with lib;
      {
        homepage = "https://github.com/dessant/web-archives#readme";
        description = "View archived and cached versions of web pages on 10+ search engines, such as the Wayback Machine, Archive․is, Google, Bing and Yandex";
        license = licenses.gpl3;
        mozPermissions = [
          "alarms"
          "contextMenus"
          "storage"
          "unlimitedStorage"
          "tabs"
          "activeTab"
          "notifications"
          "webRequest"
          "webRequestBlocking"
          "<all_urls>"
          "http://*/*"
          "https://*/*"
          "file:///*"
          ];
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
