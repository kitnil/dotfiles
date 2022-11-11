{ dns }:

with dns.lib.combinators;

rec {
  TTL = 60 * 60;

  SOA = {
    nameServer = "ns1.wugi.info.";
    adminEmail = "admin.wugi.info.";
    serial = 2022111100;
    refresh = 10800;
    retry = 3600;
    expire = 432000;
    minimum = 3600;
  };

  NS = [
    "ns1.wugi.info."
    "ns2.wugi.info."
  ];

  MX = [{
    exchange = "smtp.wugi.info.";
    preference = 10;
  }];

  TXT = [
    "v=spf1 ip4:78.108.82.44/32 -all"
  ];

  A = [ "78.108.82.44" ];

  subdomains = rec {
    ns1 = { inherit A; };
    ns2 = vm2;
    vm2.A = [ "78.108.92.69" ];
    "_dmarc" = {
      TXT = [ "v=DMARC1; p=none" ];
    };
    "_domainkey" = {
      subdomains = {
        "default" = {
          TXT = [ "v=DKIM1; h=sha256; k=rsa; p=MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAnHhmUC1AWAujQH/AAGTWxDmUayNVnstXCRQG6PC5sxMj2Gdo1rqyYlILAjndV5gPoAey+RCMglnUaE0kep0fRFwt96xslEqzEIQeEUtMUa21SY0IQzMCJFTzDkkXOCQD4coc1MvAEqhxkdathS0aiTj9HnzDyF2YtOKZe0ja9qlMpe8YyPg+3BkPsSlmkspuwzXIzLUslZjlk4cmcEkvk/Lg0JsLrqTgmA82tMMCdbb7jYSTwslMAv5/nesSAFa0kse1hNpEYdiFgdu6PIVci1Iq6N/UsGwQ6AzcJCFepbR00sKkHNpMXT+7i17GthuVGtFtT4ECAejiLnLf0G/XRwIDAQAB" ];
        };
      };
    };
    smtp = {
      inherit A;
      subdomains = {
        "_domainkey" = {
          subdomains = {
            default = {
              TXT = [ "v=DKIM1; h=sha256; k=rsa; p=MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAnHhmUC1AWAujQH/AAGTWxDmUayNVnstXCRQG6PC5sxMj2Gdo1rqyYlILAjndV5gPoAey+RCMglnUaE0kep0fRFwt96xslEqzEIQeEUtMUa21SY0IQzMCJFTzDkkXOCQD4coc1MvAEqhxkdathS0aiTj9HnzDyF2YtOKZe0ja9qlMpe8YyPg+3BkPsSlmkspuwzXIzLUslZjlk4cmcEkvk/Lg0JsLrqTgmA82tMMCdbb7jYSTwslMAv5/nesSAFa0kse1hNpEYdiFgdu6PIVci1Iq6N/UsGwQ6AzcJCFepbR00sKkHNpMXT+7i17GthuVGtFtT4ECAejiLnLf0G/XRwIDAQAB" ];
            };
          };
        };
      };
    };
    "_gitlab-pages-verification-code" = {
      TXT = [ "gitlab-pages-verification-code=a3f996c23226d3080ed3ce22d7d21ebf" ];
    };
    blog = {
      CNAME = [ "wigust.gitlab.io." ];
      subdomains = {
        "_gitlab-pages-verification-code" = {
          TXT = [ "gitlab-pages-verification-code=ed398cc16f62b90d0e014a7b2d541429" ];
        };
      };
    };
    back = { inherit A; };
    cgit = guix;
    docker-registry = guix;
    guix.A = [ "88.201.161.72" ];
    file = { inherit A; };
    githunt = { inherit A; };
    homer = { inherit A; };
    imap = { inherit A; };
    iso = guix;
    peertube = guix;
    xmpp = { inherit A; };
    mjru.NS = [ "ns.majordomo.ru." ];
    cuirass = guix;
    mg = {
      subdomains = {
        email.CNAME = [ "eu.mailgun.org." ];
      };
    };
    git = guix;
    jenkins = guix;
    monitor = guix;
    syncthing = guix;
    torrent = guix;
    webssh = guix;
    vm1 = {
      inherit A;
      subdomains = {
        webssh = {
          inherit A;
        };
      };
    };
    www = { inherit A; };
  };
}
