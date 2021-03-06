# Autogenerated by https://github.com/teh/jenkins-plugins-to-nix
# Do not edit by hand, changes will be overwritten!
{ stdenv, fetchurl }:
let mkJenkinsPlugin = { name, src }: stdenv.mkDerivation {
  name = name;
  src = src;
  phases = "installPhase";
  installPhase = ''
    mkdir $out
    cp --recursive $src $out
  '';
};
in rec {
  popper-api = mkJenkinsPlugin {
    name = "popper-api";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/popper-api/1.16.0-7/popper-api.hpi";
      sha256 = "0n452y7fgj4b741mx1iccy0h3idcqfdzhpzlyfa5pxvrs2lnni8b";
    };
  };
  ace-editor = mkJenkinsPlugin {
    name = "ace-editor";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/ace-editor/1.1/ace-editor.hpi";
      sha256 = "1lmsk9gfkngymdnykbjg2rjil7wfisj9wmaz39c732iwi4l71jdb";
    };
  };
  analysis-core = mkJenkinsPlugin {
    name = "analysis-core";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/analysis-core/1.96/analysis-core.hpi";
      sha256 = "07wcsn2g10mnj4szncj6y5azq4f9z32aqy7jpinmvjh7vc5nksg7";
    };
  };
  analysis-model-api = mkJenkinsPlugin {
    name = "analysis-model-api";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/analysis-model-api/9.3.1/analysis-model-api.hpi";
      sha256 = "04sj9iynb0w7vhfmydnfnlsvmwzd4d627vax5c3cfcy42hm9knz4";
    };
  };
  android-emulator = mkJenkinsPlugin {
    name = "android-emulator";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/android-emulator/3.1.2/android-emulator.hpi";
      sha256 = "0r9ffrd8g03x68qskz386aqyyzvxsvxlx7vfffigl67nmgmg25pd";
    };
  };
  android-lint = mkJenkinsPlugin {
    name = "android-lint";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/android-lint/2.6/android-lint.hpi";
      sha256 = "0sggii73jk519w7jd6q2dnk1j6vql3129jddqxzd93wk1fhmywnr";
    };
  };
  ansible = mkJenkinsPlugin {
    name = "ansible";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/ansible/1.1/ansible.hpi";
      sha256 = "1mm70glxj3l3r227bh5vjypangz27pg73dpi2n0kfr0q4rh2mdvv";
    };
  };
  ansicolor = mkJenkinsPlugin {
    name = "ansicolor";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/ansicolor/0.7.3/ansicolor.hpi";
      sha256 = "01s0xbfppzs3mcmb8nx2saycal75xn9m07s55ai9jk4fvgpfx2np";
    };
  };
  antisamy-markup-formatter = mkJenkinsPlugin {
    name = "antisamy-markup-formatter";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/antisamy-markup-formatter/2.1/antisamy-markup-formatter.hpi";
      sha256 = "1fh2cxx1y8f71w9r12d7cwrh6ka616a977wlrzi0gb6cg69xsbw1";
    };
  };
  apache-httpcomponents-client-4-api = mkJenkinsPlugin {
    name = "apache-httpcomponents-client-4-api";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/apache-httpcomponents-client-4-api/4.5.10-2.0/apache-httpcomponents-client-4-api.hpi";
      sha256 = "0d4kk456ngdqphrfw5i1lhk574v7x3l89qxxbcycrc07zmympj46";
    };
  };
  authentication-tokens = mkJenkinsPlugin {
    name = "authentication-tokens";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/authentication-tokens/1.4/authentication-tokens.hpi";
      sha256 = "0v6ccrg2ggx496h0z2yc3ahcz7vskfw09aq8hsd6pv1wc16k9wxb";
    };
  };
  basic-branch-build-strategies = mkJenkinsPlugin {
    name = "basic-branch-build-strategies";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/basic-branch-build-strategies/1.3.2/basic-branch-build-strategies.hpi";
      sha256 = "1dvfv7n83vv5qy3nnpdyp0hii1vw7398m4npr7y9rq5ql7wppqyf";
    };
  };
  blueocean-autofavorite = mkJenkinsPlugin {
    name = "blueocean-autofavorite";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/blueocean-autofavorite/1.2.4/blueocean-autofavorite.hpi";
      sha256 = "0jq7f2bzffpvvy9jrpy0w2jynl7nx12ca10xm3rwz1rkagqhvbwa";
    };
  };
  blueocean-bitbucket-pipeline = mkJenkinsPlugin {
    name = "blueocean-bitbucket-pipeline";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/blueocean-bitbucket-pipeline/1.24.3/blueocean-bitbucket-pipeline.hpi";
      sha256 = "06iyb0jk67radyfappm7ndd7y6xcwmbjjy4ah2l0qyyakn03blzn";
    };
  };
  blueocean-commons = mkJenkinsPlugin {
    name = "blueocean-commons";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/blueocean-commons/1.24.3/blueocean-commons.hpi";
      sha256 = "1mpjxcdbf0sngk7srmmjr6lc805lp43dvii236mlf6r6151qq69a";
    };
  };
  blueocean-config = mkJenkinsPlugin {
    name = "blueocean-config";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/blueocean-config/1.24.3/blueocean-config.hpi";
      sha256 = "1mrlfjipy2yv2y5sf9m8izkk87rwv4vnhxw1sr33hvc9py1hmxz9";
    };
  };
  blueocean-core-js = mkJenkinsPlugin {
    name = "blueocean-core-js";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/blueocean-core-js/1.24.3/blueocean-core-js.hpi";
      sha256 = "0f0364mnmlbwciqqa00519ml1xrihyhdplidmvg3iywygkhqlkkx";
    };
  };
  blueocean-dashboard = mkJenkinsPlugin {
    name = "blueocean-dashboard";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/blueocean-dashboard/1.24.3/blueocean-dashboard.hpi";
      sha256 = "1bazjzsghhxvzr619ak03znx12yh5gr3zw346bm0k80wk4x7s2vp";
    };
  };
  blueocean-display-url = mkJenkinsPlugin {
    name = "blueocean-display-url";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/blueocean-display-url/2.4.0/blueocean-display-url.hpi";
      sha256 = "10zdym6ssmwfyy407ikj68zlbmjrash45a9nnqxf2i84v2zn4vnd";
    };
  };
  blueocean-events = mkJenkinsPlugin {
    name = "blueocean-events";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/blueocean-events/1.24.3/blueocean-events.hpi";
      sha256 = "026jmvr43fs0f51fxgdbvsxlfvw1c4j5zfdj8sy87wbg0nss6b0y";
    };
  };
  blueocean-executor-info = mkJenkinsPlugin {
    name = "blueocean-executor-info";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/blueocean-executor-info/1.24.3/blueocean-executor-info.hpi";
      sha256 = "1paiaav38yhi7azj3y54yn2xh7p1l02gzkmxya0lxvs2fr44a2k2";
    };
  };
  blueocean-git-pipeline = mkJenkinsPlugin {
    name = "blueocean-git-pipeline";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/blueocean-git-pipeline/1.24.3/blueocean-git-pipeline.hpi";
      sha256 = "1db8585knlpzh2iaidm6n82xzkg4lvqxlqs31x2ikjkahxrfy2q8";
    };
  };
  blueocean-github-pipeline = mkJenkinsPlugin {
    name = "blueocean-github-pipeline";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/blueocean-github-pipeline/1.24.3/blueocean-github-pipeline.hpi";
      sha256 = "1gs29lwfxcymnra5wllklwz6a69wb58zlc3bnrhxd93mii3whjzs";
    };
  };
  blueocean-i18n = mkJenkinsPlugin {
    name = "blueocean-i18n";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/blueocean-i18n/1.24.3/blueocean-i18n.hpi";
      sha256 = "0yy7z0c2yr5hlzd6gcwv1yqvh0x2dfw86hrzabmqgqy527g4f3na";
    };
  };
  blueocean-jira = mkJenkinsPlugin {
    name = "blueocean-jira";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/blueocean-jira/1.24.3/blueocean-jira.hpi";
      sha256 = "11g7y12pvl7bf0ni6cpqr7kipqys8iglxm50j3178w62vg5yq4n6";
    };
  };
  blueocean-jwt = mkJenkinsPlugin {
    name = "blueocean-jwt";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/blueocean-jwt/1.24.3/blueocean-jwt.hpi";
      sha256 = "178cys4qdyb22xdgj0sw2jgd1ia5v9k4z1h8wpfbr62hln0zzd6z";
    };
  };
  blueocean-personalization = mkJenkinsPlugin {
    name = "blueocean-personalization";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/blueocean-personalization/1.24.3/blueocean-personalization.hpi";
      sha256 = "1ly9fyf98805igx5c134cn276h0mhas33ghgq61aj18shrslnsj4";
    };
  };
  blueocean-pipeline-api-impl = mkJenkinsPlugin {
    name = "blueocean-pipeline-api-impl";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/blueocean-pipeline-api-impl/1.24.3/blueocean-pipeline-api-impl.hpi";
      sha256 = "0yl5z3if0j0v3appqfzsh0vjgv21y5w5zxfdq00cxcfarzy887a0";
    };
  };
  blueocean-pipeline-editor = mkJenkinsPlugin {
    name = "blueocean-pipeline-editor";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/blueocean-pipeline-editor/1.24.3/blueocean-pipeline-editor.hpi";
      sha256 = "19wnlnyinrkdlf2i4m7j5n8ws4331jv7qp6hsqy7vr5l57nlf3g6";
    };
  };
  blueocean-pipeline-scm-api = mkJenkinsPlugin {
    name = "blueocean-pipeline-scm-api";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/blueocean-pipeline-scm-api/1.24.3/blueocean-pipeline-scm-api.hpi";
      sha256 = "181iq9hc35lcbch9i53hp49d8bhs5v842sq0dd0lbd5iq25zcbsy";
    };
  };
  blueocean-rest-impl = mkJenkinsPlugin {
    name = "blueocean-rest-impl";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/blueocean-rest-impl/1.24.3/blueocean-rest-impl.hpi";
      sha256 = "01zgrzkvzv6mh4k1zbbfhc4lvlmsf5dmhq122xyac3hq5667k51y";
    };
  };
  blueocean-rest = mkJenkinsPlugin {
    name = "blueocean-rest";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/blueocean-rest/1.24.3/blueocean-rest.hpi";
      sha256 = "17rsnx1jc3a9dan665svkvp5xrhbifqba9fsmbqf335dpqf1nq9r";
    };
  };
  blueocean-web = mkJenkinsPlugin {
    name = "blueocean-web";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/blueocean-web/1.24.3/blueocean-web.hpi";
      sha256 = "150f71kds2h114w4vfw06rhcqvmdm6l6mjgwm4875c1izs3708a4";
    };
  };
  blueocean = mkJenkinsPlugin {
    name = "blueocean";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/blueocean/1.24.3/blueocean.hpi";
      sha256 = "0a3hfmw6ngzy8siwyz2hgdw336q95shkzby8lkfm2f2hdp66p0j0";
    };
  };
  bootstrap4-api = mkJenkinsPlugin {
    name = "bootstrap4-api";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/bootstrap4-api/4.5.3-1/bootstrap4-api.hpi";
      sha256 = "1g73ph519ngx2mb5p690adsfm2qi4s243k8kzyi55l7m8inp9s9s";
    };
  };
  branch-api = mkJenkinsPlugin {
    name = "branch-api";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/branch-api/2.6.1/branch-api.hpi";
      sha256 = "0qxhp451is7h4fs8vrfi5y9c4rdim82w2r539v0gq4v3ilzd3ywq";
    };
  };
  checks-api = mkJenkinsPlugin {
    name = "checks-api";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/checks-api/1.1.0/checks-api.hpi";
      sha256 = "0v5asjs375sa58lrprbh1v3grx1pljzs4n47wl89qyqwwcdv1gc0";
    };
  };
  cloudbees-bitbucket-branch-source = mkJenkinsPlugin {
    name = "cloudbees-bitbucket-branch-source";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/cloudbees-bitbucket-branch-source/2.9.4/cloudbees-bitbucket-branch-source.hpi";
      sha256 = "1l97hjyhpdnw28rjw0b0c94hjv9vmib3p5fxk1lgxw2a5s708v9r";
    };
  };
  cloudbees-folder = mkJenkinsPlugin {
    name = "cloudbees-folder";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/cloudbees-folder/6.14/cloudbees-folder.hpi";
      sha256 = "1xgcfmcbvqhzg5b6h0p96npd4g8jxzfgnv8k5r80v071igb8s44b";
    };
  };
  configuration-as-code = mkJenkinsPlugin {
    name = "configuration-as-code";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/configuration-as-code/1.46/configuration-as-code.hpi";
      sha256 = "11cjl7ym24p8kmq72lljhh6zfwvqxh4dqjkkqvk2fnr6p8x9q8vn";
    };
  };
  credentials-binding = mkJenkinsPlugin {
    name = "credentials-binding";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/credentials-binding/1.24/credentials-binding.hpi";
      sha256 = "03n8kv5bg713ydpnyispm6aqlc2csbdlwawz3gjf0q88429p4wcn";
    };
  };
  credentials = mkJenkinsPlugin {
    name = "credentials";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/credentials/2.3.13/credentials.hpi";
      sha256 = "1qc8zsrndlm2hhyxf1c0h30scb6sbzmqndbnfm8c4cgpnx57rrh8";
    };
  };
  data-tables-api = mkJenkinsPlugin {
    name = "data-tables-api";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/data-tables-api/1.10.21-3/data-tables-api.hpi";
      sha256 = "00qgql6n3bgmvzhm9f2np6b8a8mw4wa4b5zrf9knzhias1g05dxv";
    };
  };
  display-url-api = mkJenkinsPlugin {
    name = "display-url-api";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/display-url-api/2.3.3/display-url-api.hpi";
      sha256 = "04wvpph0j7wj68a9aa54d48sh1xqacaxcz1zx2my0d34aivpykkm";
    };
  };
  docker-commons = mkJenkinsPlugin {
    name = "docker-commons";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/docker-commons/1.17/docker-commons.hpi";
      sha256 = "1x3pkzikmni77xmpng659cq1b7mpn54g9y0ijrmn2q00s24ki07q";
    };
  };
  docker-workflow = mkJenkinsPlugin {
    name = "docker-workflow";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/docker-workflow/1.24/docker-workflow.hpi";
      sha256 = "1chxynbmy4257dwqiws4cp7blbhvrgxjyrxqwmfing7q06r87q2r";
    };
  };
  durable-task = mkJenkinsPlugin {
    name = "durable-task";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/durable-task/1.35/durable-task.hpi";
      sha256 = "16jfijgknkyw5d59vvc0sjvxgz2fdxrsg5y0a330a9i25wz46dqf";
    };
  };
  echarts-api = mkJenkinsPlugin {
    name = "echarts-api";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/echarts-api/4.9.0-2/echarts-api.hpi";
      sha256 = "0539hikfsx9xlk3ay3ibvpjjp39spnlb2bg2fa1nk7iy9lbml8k6";
    };
  };
  embeddable-build-status = mkJenkinsPlugin {
    name = "embeddable-build-status";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/embeddable-build-status/2.0.3/embeddable-build-status.hpi";
      sha256 = "1dqkbc2d1hpn398x890p7mm4wsg8l64gsgy3qg8w28dk36m0rkb1";
    };
  };
  extra-columns = mkJenkinsPlugin {
    name = "extra-columns";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/extra-columns/1.22/extra-columns.hpi";
      sha256 = "0bzx6q3p9anfpqplfwjvk2vbpd6xaq9vmn3pcgw9cj8z9wrln59v";
    };
  };
  favorite = mkJenkinsPlugin {
    name = "favorite";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/favorite/2.3.2/favorite.hpi";
      sha256 = "1cy16pn1zxcg3kj0339kl3m41f9j8svd50caavxj07jlykfn65fm";
    };
  };
  font-awesome-api = mkJenkinsPlugin {
    name = "font-awesome-api";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/font-awesome-api/5.15.1-1/font-awesome-api.hpi";
      sha256 = "1v3wyf1v8cx66h5v6hv6ji6psly3asag7fy9dy1nsdd1axk6dm3r";
    };
  };
  forensics-api = mkJenkinsPlugin {
    name = "forensics-api";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/forensics-api/0.7.0/forensics-api.hpi";
      sha256 = "1cv51mhbmjdk4fvy6d8wi5bb02yw3yj8wggvzw9bbg9bdzvc0rhn";
    };
  };
  git-client = mkJenkinsPlugin {
    name = "git-client";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/git-client/3.5.1/git-client.hpi";
      sha256 = "1ihai9kh4yba2aj26hgbmiw8ra0imimsyngg6f45fs718w9y142s";
    };
  };
  git-server = mkJenkinsPlugin {
    name = "git-server";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/git-server/1.9/git-server.hpi";
      sha256 = "1s9xhc2xrl3axdwaqw6dnla061zca8hr2pxb557ysmba1drnp85f";
    };
  };
  git = mkJenkinsPlugin {
    name = "git";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/git/4.4.5/git.hpi";
      sha256 = "118rhvvqzbqa14db4rz70sydkn0k50yx0s2jgh4cg8x24dhyfj4x";
    };
  };
  github-api = mkJenkinsPlugin {
    name = "github-api";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/github-api/1.116/github-api.hpi";
      sha256 = "0i5jsmjy876i8vdz1fc5n3zxak1px7059fgyfbklrl3y8bj2pkbn";
    };
  };
  github-branch-source = mkJenkinsPlugin {
    name = "github-branch-source";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/github-branch-source/2.9.1/github-branch-source.hpi";
      sha256 = "0nwalsw9g1ffgw5784l8hsmq5a3xh7r9i9k2v8b6c46h89vksicx";
    };
  };
  github = mkJenkinsPlugin {
    name = "github";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/github/1.32.0/github.hpi";
      sha256 = "09vrqsfcxvbsmdigdjvlhwpk1wjsgl4rc778mydsl1imb0p78fbn";
    };
  };
  gitlab-hook = mkJenkinsPlugin {
    name = "gitlab-hook";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/gitlab-hook/1.4.2/gitlab-hook.hpi";
      sha256 = "07zgs1mfcgcrskj4myjgkd9jl19dfn1prj9bj4f17qa8ly641vlc";
    };
  };
  gitlab-plugin = mkJenkinsPlugin {
    name = "gitlab-plugin";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/gitlab-plugin/1.5.13/gitlab-plugin.hpi";
      sha256 = "0bavxc9mqm316dlh4xapyryb9zvwxa3jsgmvb154c0ykp7n8i0xn";
    };
  };
  gradle = mkJenkinsPlugin {
    name = "gradle";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/gradle/1.36/gradle.hpi";
      sha256 = "0syzg3s1wxjd3vihx1fq916srmaxl1d9riryznrcaf2bifaq95ap";
    };
  };
  greenballs = mkJenkinsPlugin {
    name = "greenballs";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/greenballs/1.15/greenballs.hpi";
      sha256 = "1ywn88f1wziyjsv0p29q1yhrh27mgvc126bf4vq4d972kkxj4dvc";
    };
  };
  handlebars = mkJenkinsPlugin {
    name = "handlebars";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/handlebars/1.1.1/handlebars.hpi";
      sha256 = "1ykajqc7m1bglnirm8c7jfhxg2m7797zkpm8q37acyx1xjrwfp5w";
    };
  };
  handy-uri-templates-2-api = mkJenkinsPlugin {
    name = "handy-uri-templates-2-api";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/handy-uri-templates-2-api/2.1.8-1.0/handy-uri-templates-2-api.hpi";
      sha256 = "1s3g3mnzphxi1pk7y8m3w83wmvvkbq5fxs7i1b2cg5gn16c8sryd";
    };
  };
  htmlpublisher = mkJenkinsPlugin {
    name = "htmlpublisher";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/htmlpublisher/1.23/htmlpublisher.hpi";
      sha256 = "1wsyh0zxh2dxdhbx4mk1jv73cwlpkysdk627x3n1zskaakh8ckm1";
    };
  };
  jackson2-api = mkJenkinsPlugin {
    name = "jackson2-api";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/jackson2-api/2.11.3/jackson2-api.hpi";
      sha256 = "1mx9pqf0fhx4xlkp8yx31vg89nfa6s6bbfy5d97l0vn55i2ldj7p";
    };
  };
  jdk-tool = mkJenkinsPlugin {
    name = "jdk-tool";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/jdk-tool/1.4/jdk-tool.hpi";
      sha256 = "1riyn727lb2sanpdc4m4jxvv1hai1w3mjcn0ilqpslz1pwg1aybi";
    };
  };
  jenkins-design-language = mkJenkinsPlugin {
    name = "jenkins-design-language";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/jenkins-design-language/1.24.3/jenkins-design-language.hpi";
      sha256 = "0cfw6wpf4xrlk8rx108j6fpzzi0gwbhks1s3v9rfbdjrc1xlsy5d";
    };
  };
  jira = mkJenkinsPlugin {
    name = "jira";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/jira/3.1.3/jira.hpi";
      sha256 = "09dyhcj5ixnnglmr8mnfnfx08mxcy9l3i2rddpyh0r2z3ap9pn0q";
    };
  };
  job-dsl = mkJenkinsPlugin {
    name = "job-dsl";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/job-dsl/1.77/job-dsl.hpi";
      sha256 = "08mj4x2n5q66lkpax1rvflqky4s1xq6bm5fnpqy3wnfhfmfs6wbh";
    };
  };
  jquery-detached = mkJenkinsPlugin {
    name = "jquery-detached";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/jquery-detached/1.2.1/jquery-detached.hpi";
      sha256 = "1h0igw6rdhiip145ad98pv45vlr11wapbjvxrgzmf5f1436p6lm0";
    };
  };
  jquery3-api = mkJenkinsPlugin {
    name = "jquery3-api";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/jquery3-api/3.5.1-2/jquery3-api.hpi";
      sha256 = "176p2v23y934dxy3pkycvnhdszpf5gq392r2x20kn9q075y5n3a8";
    };
  };
  jsch = mkJenkinsPlugin {
    name = "jsch";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/jsch/0.1.55.2/jsch.hpi";
      sha256 = "0i7w4f4iv2ji6swak7b9720414c8dj32zflqmpk0md1ywkw4piyd";
    };
  };
  junit = mkJenkinsPlugin {
    name = "junit";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/junit/1.43/junit.hpi";
      sha256 = "01ynrgg7i06w0a2ylyqzwm4nrgnqh1az3cpggz77518svz3npi6f";
    };
  };
  lockable-resources = mkJenkinsPlugin {
    name = "lockable-resources";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/lockable-resources/2.10/lockable-resources.hpi";
      sha256 = "0006vdfcdjm28q68a8rjqkcnrpl1ik8a9ycgc1m90dddy8h43n6q";
    };
  };
  mailer = mkJenkinsPlugin {
    name = "mailer";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/mailer/1.32.1/mailer.hpi";
      sha256 = "0s6kl910wi0rgrk4h6xphfb5v503r7j53b2mmp69ya7wgsl5w10b";
    };
  };
  matrix-project = mkJenkinsPlugin {
    name = "matrix-project";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/matrix-project/1.18/matrix-project.hpi";
      sha256 = "1pa6b1vmd46fyajmh03fgvz0mrxpdjpqx8ja2j911irl0y2sd7d6";
    };
  };
  maven-plugin = mkJenkinsPlugin {
    name = "maven-plugin";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/maven-plugin/3.8/maven-plugin.hpi";
      sha256 = "04vypibwm2nqcwzpyjbglma84vhxa94r3zr6nvi1wf964sx34wmb";
    };
  };
  mercurial = mkJenkinsPlugin {
    name = "mercurial";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/mercurial/2.12/mercurial.hpi";
      sha256 = "10ibf0997a4m3pvwnc5awv0wpqcq6p4ax2qw495k8kzni7lkxa4f";
    };
  };
  momentjs = mkJenkinsPlugin {
    name = "momentjs";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/momentjs/1.1.1/momentjs.hpi";
      sha256 = "0j28ib5mn5yf9n2rnhdkp7yvzgy02cgxxiqdj0ggfmgz9hk2sg6a";
    };
  };
  nexus-jenkins-plugin = mkJenkinsPlugin {
    name = "nexus-jenkins-plugin";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/nexus-jenkins-plugin/3.9.20200722-164144.e3a1be0/nexus-jenkins-plugin.hpi";
      sha256 = "1zscsg8vvh9n26wqq5ykp21yzxclfn4fhspk2x4dd71mvinqxgx4";
    };
  };
  okhttp-api = mkJenkinsPlugin {
    name = "okhttp-api";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/okhttp-api/3.14.9/okhttp-api.hpi";
      sha256 = "1z8js22p9rvac2vmxwh80ly0ksp2jfhawpz25r4z0h6859j1nr6z";
    };
  };
  pipeline-build-step = mkJenkinsPlugin {
    name = "pipeline-build-step";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/pipeline-build-step/2.13/pipeline-build-step.hpi";
      sha256 = "0c6iv0q5kb92msh7917szlhmbkdi39gqa61sbs00f3xhx9dd6wy7";
    };
  };
  pipeline-graph-analysis = mkJenkinsPlugin {
    name = "pipeline-graph-analysis";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/pipeline-graph-analysis/1.10/pipeline-graph-analysis.hpi";
      sha256 = "1bicjc4njsklc23s65zg0a2k2k665bw1v24b6c73mwz998s9fa7h";
    };
  };
  pipeline-input-step = mkJenkinsPlugin {
    name = "pipeline-input-step";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/pipeline-input-step/2.12/pipeline-input-step.hpi";
      sha256 = "1b1v193szz9knjyfa826lr6593m66cyw92paaz6lvglxq11wk0pb";
    };
  };
  pipeline-milestone-step = mkJenkinsPlugin {
    name = "pipeline-milestone-step";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/pipeline-milestone-step/1.3.1/pipeline-milestone-step.hpi";
      sha256 = "086byw1v9dp24yy6sh1qwf2g0l7wiamh7r2gl5h1r0jnnqlnd8nk";
    };
  };
  pipeline-model-api = mkJenkinsPlugin {
    name = "pipeline-model-api";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/pipeline-model-api/1.7.2/pipeline-model-api.hpi";
      sha256 = "0asb4i7k22am8gamrv9ynnhw0ckawp0zzkhyjq6by3lkfpngvy82";
    };
  };
  pipeline-model-declarative-agent = mkJenkinsPlugin {
    name = "pipeline-model-declarative-agent";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/pipeline-model-declarative-agent/1.1.1/pipeline-model-declarative-agent.hpi";
      sha256 = "1vyl2h92ljmgbz712hnxsfwlhca15h03lfzr224mcpxzfcwhqm6m";
    };
  };
  pipeline-model-definition = mkJenkinsPlugin {
    name = "pipeline-model-definition";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/pipeline-model-definition/1.7.2/pipeline-model-definition.hpi";
      sha256 = "0nl958664397g42y9bn8xnpyfyb1kh0pg04l5vifbj63d9slv180";
    };
  };
  pipeline-model-extensions = mkJenkinsPlugin {
    name = "pipeline-model-extensions";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/pipeline-model-extensions/1.7.2/pipeline-model-extensions.hpi";
      sha256 = "0ahzcnf47ls1fanf7qbq7g04bfaqij3sjxrhri975c87jyq4nwxr";
    };
  };
  pipeline-rest-api = mkJenkinsPlugin {
    name = "pipeline-rest-api";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/pipeline-rest-api/2.18/pipeline-rest-api.hpi";
      sha256 = "1hdvklscckz16kr68i0f06w07v05fmyikic9jpkslbw9zbjjnv7f";
    };
  };
  pipeline-stage-step = mkJenkinsPlugin {
    name = "pipeline-stage-step";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/pipeline-stage-step/2.5/pipeline-stage-step.hpi";
      sha256 = "08v5djzlfdxa81vlw9jd9k7hlkw15ygq7ywfjyxa0smx9lbn53x9";
    };
  };
  pipeline-stage-tags-metadata = mkJenkinsPlugin {
    name = "pipeline-stage-tags-metadata";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/pipeline-stage-tags-metadata/1.7.2/pipeline-stage-tags-metadata.hpi";
      sha256 = "0fppg79p2i8z8h2fp1yzfq6zq026db86ajnvbypxh4y5dyjv0gsn";
    };
  };
  pipeline-stage-view = mkJenkinsPlugin {
    name = "pipeline-stage-view";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/pipeline-stage-view/2.18/pipeline-stage-view.hpi";
      sha256 = "13hb2qk8fvf2kjmqhl4ij0gnmalihdv16ir2d6j0z08989x7qdax";
    };
  };
  pipeline-utility-steps = mkJenkinsPlugin {
    name = "pipeline-utility-steps";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/pipeline-utility-steps/2.6.1/pipeline-utility-steps.hpi";
      sha256 = "035vchrbl06xsvkp8xyzcp9dvdrspghqfqcqr1f4ql5wcm00qw8i";
    };
  };
  plain-credentials = mkJenkinsPlugin {
    name = "plain-credentials";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/plain-credentials/1.7/plain-credentials.hpi";
      sha256 = "1a1v2r6q1gidd6sg20ybq7ar7abyvwgm6nv9cgvcrnlpndkcm0js";
    };
  };
  plugin-util-api = mkJenkinsPlugin {
    name = "plugin-util-api";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/plugin-util-api/1.4.0/plugin-util-api.hpi";
      sha256 = "03g8dka112m17br889nsi9qfrqhfxdkkk83sk26x6nm1rhjnq6jw";
    };
  };
  port-allocator = mkJenkinsPlugin {
    name = "port-allocator";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/port-allocator/1.8/port-allocator.hpi";
      sha256 = "0zzn9zggq4z9zx24lcvfq978x9b6p0dri893ilmz66q22lxina44";
    };
  };
  pubsub-light = mkJenkinsPlugin {
    name = "pubsub-light";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/pubsub-light/1.13/pubsub-light.hpi";
      sha256 = "0v28ppa7z8zmbx21lmcay85xjnhjzc9nvq60fniqkdb6gwn68z8a";
    };
  };
  rebuild = mkJenkinsPlugin {
    name = "rebuild";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/rebuild/1.31/rebuild.hpi";
      sha256 = "1lsq0kj4kmrqhz2fdbizkfrjappf6pjx2gx0mkd179l3r2yxfrg1";
    };
  };
  resource-disposer = mkJenkinsPlugin {
    name = "resource-disposer";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/resource-disposer/0.14/resource-disposer.hpi";
      sha256 = "1blz9aximnzpfqwlgghrlli9r0fazzhp8v9csialck18jprcli10";
    };
  };
  ruby-runtime = mkJenkinsPlugin {
    name = "ruby-runtime";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/ruby-runtime/0.12/ruby-runtime.hpi";
      sha256 = "0m23s30frn7hvrlxscia2psncb13m79kxkvijmnk36bs4dlsmypq";
    };
  };
  scm-api = mkJenkinsPlugin {
    name = "scm-api";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/scm-api/2.6.4/scm-api.hpi";
      sha256 = "1p3nnsk5rfbsi81wk28c1m7a0llz09l9mb7sz6nxay36nkjblp4k";
    };
  };
  script-security = mkJenkinsPlugin {
    name = "script-security";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/script-security/1.75/script-security.hpi";
      sha256 = "1f64362pf3p8bz7pz910dkls9hs1mqqrr8d4pfanchm85g2yvag3";
    };
  };
  slack = mkJenkinsPlugin {
    name = "slack";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/slack/2.43/slack.hpi";
      sha256 = "1v0awycrlbihrcsq9hlbdci1hw0a3kvkqja7m72a0ynqdxyjalrf";
    };
  };
  snakeyaml-api = mkJenkinsPlugin {
    name = "snakeyaml-api";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/snakeyaml-api/1.27.0/snakeyaml-api.hpi";
      sha256 = "1q22hpgqvi7sh149hz91mz3bhvviz8pkvwsrr43h0i8y9kj0wm8j";
    };
  };
  sse-gateway = mkJenkinsPlugin {
    name = "sse-gateway";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/sse-gateway/1.24/sse-gateway.hpi";
      sha256 = "076mr3134cqazfnd5b69m9n3h5p05kij4aic0ni8j4x4m340k4lf";
    };
  };
  ssh-credentials = mkJenkinsPlugin {
    name = "ssh-credentials";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/ssh-credentials/1.18.1/ssh-credentials.hpi";
      sha256 = "14h46xff94an6wjpkn7gp6rr1wzard12ls5nz862czvkhk20ifcd";
    };
  };
  ssh-slaves = mkJenkinsPlugin {
    name = "ssh-slaves";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/ssh-slaves/1.31.2/ssh-slaves.hpi";
      sha256 = "0hz50b9d00caf8808w93y5y3cl4r7xixzh4gkc87qxb19v5w50cv";
    };
  };
  structs = mkJenkinsPlugin {
    name = "structs";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/structs/1.20/structs.hpi";
      sha256 = "0fpb7xvlmmi64yf39xsj283hg4yx2sksxmv2fir6maipd8sn2y3y";
    };
  };
  timestamper = mkJenkinsPlugin {
    name = "timestamper";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/timestamper/1.11.8/timestamper.hpi";
      sha256 = "0rihglaq7v1g2770lglgi51z41l9g7v3a7qlvglqxnb8scwpjvc8";
    };
  };
  token-macro = mkJenkinsPlugin {
    name = "token-macro";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/token-macro/2.12/token-macro.hpi";
      sha256 = "0r7lzj5450cvg3yx5g3s0vm8ivhbdqwkypnx1dbnb3wg7n8m1dh5";
    };
  };
  trilead-api = mkJenkinsPlugin {
    name = "trilead-api";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/trilead-api/1.0.12/trilead-api.hpi";
      sha256 = "1psmd26i451160z4vkg519jg43lc2zh1pyhm68qq1aag1rj046v9";
    };
  };
  variant = mkJenkinsPlugin {
    name = "variant";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/variant/1.3/variant.hpi";
      sha256 = "10mkrprvbmq0c8q513jk43w5rc87fr5ghv85n3xizfg4712hy4ym";
    };
  };
  view-job-filters = mkJenkinsPlugin {
    name = "view-job-filters";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/view-job-filters/2.3/view-job-filters.hpi";
      sha256 = "1fa3l0kqs7pdfjxis0q89fc9kkq61dxfxj7vrlnjdp0qyh3d8kll";
    };
  };
  warnings-ng = mkJenkinsPlugin {
    name = "warnings-ng";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/warnings-ng/8.4.4/warnings-ng.hpi";
      sha256 = "1fdbx98is7ffv6n4d219i900c4qh6w3qh3w6ypy0aqs2yf2k1c27";
    };
  };
  workflow-aggregator = mkJenkinsPlugin {
    name = "workflow-aggregator";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/workflow-aggregator/2.6/workflow-aggregator.hpi";
      sha256 = "1xjbw85skf93cazvc1m32wh8gxlcrawxmlgj7s2qkrg6fxr4dk5x";
    };
  };
  workflow-api = mkJenkinsPlugin {
    name = "workflow-api";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/workflow-api/2.40/workflow-api.hpi";
      sha256 = "17b5469s6g2yfmhmm0xc9lapmjrmr7fcbmgsn94736sqcl5vh16p";
    };
  };
  workflow-basic-steps = mkJenkinsPlugin {
    name = "workflow-basic-steps";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/workflow-basic-steps/2.23/workflow-basic-steps.hpi";
      sha256 = "0jhwpj1s11z18bd85sr4gvyd328194zj45fh3gva8w6y17ga448v";
    };
  };
  workflow-cps-global-lib = mkJenkinsPlugin {
    name = "workflow-cps-global-lib";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/workflow-cps-global-lib/2.17/workflow-cps-global-lib.hpi";
      sha256 = "143m33ciph0cjyyrmrf7fq7zfcbpmc3bhlkvfyd042iw3jjisn35";
    };
  };
  workflow-cps = mkJenkinsPlugin {
    name = "workflow-cps";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/workflow-cps/2.84/workflow-cps.hpi";
      sha256 = "1jigli84qkl1wfnlpn9wizljjac543lmv8jzrg6r1zk5f2lhfy5y";
    };
  };
  workflow-durable-task-step = mkJenkinsPlugin {
    name = "workflow-durable-task-step";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/workflow-durable-task-step/2.36/workflow-durable-task-step.hpi";
      sha256 = "1p1afzdnaillcph6cjnxcigis4cqw7qq30lhkfqx9bmwa09i64nz";
    };
  };
  workflow-job = mkJenkinsPlugin {
    name = "workflow-job";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/workflow-job/2.40/workflow-job.hpi";
      sha256 = "1snwbc7bparwln2smkyrv1fkh1lxc9izc1b68miv255mwav1hv7m";
    };
  };
  workflow-multibranch = mkJenkinsPlugin {
    name = "workflow-multibranch";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/workflow-multibranch/2.22/workflow-multibranch.hpi";
      sha256 = "0n2k1b402yq2ym4cnrah9kiy5152r4vhbx7fsaw1n0rh8y1xc1in";
    };
  };
  workflow-scm-step = mkJenkinsPlugin {
    name = "workflow-scm-step";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/workflow-scm-step/2.11/workflow-scm-step.hpi";
      sha256 = "0z497az0r6ymzmw4958sw1wddsnl9716f708ycvwjybm5br5h2vy";
    };
  };
  workflow-step-api = mkJenkinsPlugin {
    name = "workflow-step-api";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/workflow-step-api/2.23/workflow-step-api.hpi";
      sha256 = "11qjxa8x7rskwp3ciypagly06z9w1m3yzpqfz39v5gry1jz9rqz0";
    };
  };
  workflow-support = mkJenkinsPlugin {
    name = "workflow-support";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/workflow-support/3.6/workflow-support.hpi";
      sha256 = "05n6l062p42wxid1rjgj233r6a83r0dhmzadbj2svz4dxj7c0x56";
    };
  };
  ws-cleanup = mkJenkinsPlugin {
    name = "ws-cleanup";
    src = fetchurl {
      url = "https://updates.jenkins-ci.org/download/plugins/ws-cleanup/0.38/ws-cleanup.hpi";
      sha256 = "05j7ppwr7j0zp0swmi5p8pnbc46lzvh3rhkydg1ha4yybfjz9ylv";
    };
  };
}
