{ pkgs, ... }:

{
  home.file = {
    ".mozilla/firefox/tmp/mimeTypes.rdf".text = builtins.readFile ./mimeTypes.rdf;
  };
  manual.manpages.enable = false;
  programs.firefox = {
    enable = true;
    package = pkgs.firefox;
    profiles = {
      tmp = {
        name = "tmp";
        id = 0;
        isDefault = true;
        settings = {
          "browser.startup.homepage" = "about:newtab";
          "startup.homepage_welcome_url" = "about:newtab";
          "browser.startup.page" = 3;
          "general.warnOnAboutConfig" = false;
          "browser.shell.checkDefaultBrowser" = false;
          "toolkit.telemetry.reportingpolicy.firstRun" = false;
          "plugin.state.java" = 2; # IcedTea set Always Activate

          # https://bugs.launchpad.net/ubuntu/+source/sun-java6/+bug/375194
          #
          # Java is looking around for various proxy settings to try to
          # determine them, and doesn't understand Firefox's "5" type.
          # Changing "network.proxy.type" to 0 ("Direct connection, no proxy")
          # causes javaws to make a direct connection.
          # "network.proxy.type" = 0;

          "browser.search.region" = "GB";
          "distribution.searchplugins.defaultLocale" = "en-GB";
          "general.useragent.locale" = "en-GB";
          "browser.search.defaultenginename" = "Google";
          "network.proxy.type" = 1;
          "network.proxy.socks" = "windows";
          "network.proxy.socks_port" = 1080;
          "network.proxy.socks_remote_dns" = true;
        };
      };
    };
  };
}
