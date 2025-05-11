(define-module (jenkins plugins)
  #:use-module (guix download)
  #:use-module (guix packages))

(define-public %jenkins-base-plugins
  (list
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/ace-editor/1.1/ace-editor.hpi")
     (sha256 (base32 "1lmsk9gfkngymdnykbjg2rjil7wfisj9wmaz39c732iwi4l71jdb")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/ansicolor/1.0.0/ansicolor.hpi")
     (sha256 (base32 "0mj80w9lc7lq0fmvv5hlywyx27lja5yg1sfadimbb9scqsj4wxwv")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/apache-httpcomponents-client-4-api/4.5.13-1.0/apache-httpcomponents-client-4-api.hpi")
     (sha256 (base32 "0djnlrix4alhz72d5dymnrxvfv8n6xm4mi3fkcm5hg04rnpsv98z")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/async-http-client/1.9.40.0/async-http-client.hpi")
     (sha256 (base32 "1ggbr1xzvl39rhpglam5p8qg2svfm9r3wb5zmm9nly3ycwdklhdi")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/authentication-tokens/1.4/authentication-tokens.hpi")
     (sha256 (base32 "0v6ccrg2ggx496h0z2yc3ahcz7vskfw09aq8hsd6pv1wc16k9wxb")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean/1.24.8/blueocean.hpi")
     (sha256 (base32 "09lgzs0m5g7ggb850iypmcjdgpy67i0vi0277gnfqslm70z7lss0")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-autofavorite/1.2.4/blueocean-autofavorite.hpi")
     (sha256 (base32 "0jq7f2bzffpvvy9jrpy0w2jynl7nx12ca10xm3rwz1rkagqhvbwa")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-bitbucket-pipeline/1.24.8/blueocean-bitbucket-pipeline.hpi")
     (sha256 (base32 "0nvv3qhwl7xk3ai94nlgip99wz1zv46zkhwn3wkfa6wmyrn9k0s5")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-commons/1.24.8/blueocean-commons.hpi")
     (sha256 (base32 "0i2907g98sdsd3p2iyszjwgzp25vzhns0l01kin2spp835m66dv6")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-config/1.24.8/blueocean-config.hpi")
     (sha256 (base32 "1vh6rzm88gw15wb81wy2fansr1cjmcnmd477djb68rj3p4w4g6b1")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-core-js/1.24.8/blueocean-core-js.hpi")
     (sha256 (base32 "0509r0a8jxhmdb24bz4a0mpyg62772bpjm087lqdshjgxzsgxm4j")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-dashboard/1.24.8/blueocean-dashboard.hpi")
     (sha256 (base32 "0zxal9af62g8haxjqxczq3a1mr86nkygci9z81248jk3fa966qls")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-display-url/2.4.1/blueocean-display-url.hpi")
     (sha256 (base32 "00xk20kyhpikcb0jayk8j093c0sa0za3g86qgcqk1hz64619n5zl")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-events/1.24.8/blueocean-events.hpi")
     (sha256 (base32 "16v2giy5dsi18xscj6kpqawq7sz2r0j4sjpk7zyf2qwr19mj7ga3")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-executor-info/1.24.8/blueocean-executor-info.hpi")
     (sha256 (base32 "12sq2525qi1j6pc37vd8v9kskmqzl028f3jc0gwpi0i2vvnmriz9")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-git-pipeline/1.24.8/blueocean-git-pipeline.hpi")
     (sha256 (base32 "08kig7933idjal060v56sz7agbfk7zvw6q8r0aahxj1k74kwz4l7")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-github-pipeline/1.24.8/blueocean-github-pipeline.hpi")
     (sha256 (base32 "0agf2grm5clc1y4zcf9vslkgl927mbp5r6vqvc7npl86nb6hff8h")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-i18n/1.24.8/blueocean-i18n.hpi")
     (sha256 (base32 "11hhfpib11w7zx81yi6r3ndb1brsax7p4vry7a52d6309ja407ab")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-jira/1.24.8/blueocean-jira.hpi")
     (sha256 (base32 "02fkbb52wg7fppxizplmp4vnhjxrw6kc54hq6h7qs9az4vnwmxxw")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-jwt/1.24.8/blueocean-jwt.hpi")
     (sha256 (base32 "059kxdl0675kp1isfjf45vvg9i6578sxmadb6990fina0s28yrzw")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-personalization/1.24.8/blueocean-personalization.hpi")
     (sha256 (base32 "0n080rl4i9nnr5c72crhp502r3ccp3j5vs9mpbkcf9rrynhf49ma")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-pipeline-api-impl/1.24.8/blueocean-pipeline-api-impl.hpi")
     (sha256 (base32 "10w2clqrw8ai064859kn55894w80asymbncdrcvqll589r6snayr")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-pipeline-editor/1.24.8/blueocean-pipeline-editor.hpi")
     (sha256 (base32 "1i80lvafs2dlp0ijl7px9fw4i1yywzb0jz1wvjzygzc0pscmpk61")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-pipeline-scm-api/1.24.8/blueocean-pipeline-scm-api.hpi")
     (sha256 (base32 "1hs4kh915h6h3pn6r3yr7s38846p40qfi8ynfnx63hjp3pmwvhd6")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-rest/1.24.8/blueocean-rest.hpi")
     (sha256 (base32 "1ji78r7d1d325b38gv2i32023659rahfs09isv75jci8xfa3bh8h")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-rest-impl/1.24.8/blueocean-rest-impl.hpi")
     (sha256 (base32 "0vk0clycahvjgxxklm94f13yxkipvckc7np8cgrv4075k1kl98bw")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-web/1.24.8/blueocean-web.hpi")
     (sha256 (base32 "0g1la8938saqcpp3fhh3p9dlmv14mncaksq5rrqfy5cic1zwvs67")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/bootstrap4-api/4.6.0-3/bootstrap4-api.hpi")
     (sha256 (base32 "04ymgfl2gp5577lldsgbhvxz1wz5kppklxnk04ma0qhn57y5bl2y")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/bootstrap5-api/5.1.0-1/bootstrap5-api.hpi")
     (sha256 (base32 "11f0cmi5q028iv8h3jjvzjwczkl468vh15239z1k3fmimcw1gwcr")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/branch-api/2.6.5/branch-api.hpi")
     (sha256 (base32 "1dkn3sknhhsz2q0z1lcqckrd3b5nd24mixmg5a4wb2d51bbzxpab")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/caffeine-api/2.9.2-29.v717aac953ff3/caffeine-api.hpi")
     (sha256 (base32 "12p6idlm751slvdx7gh9ivl2jqpqpwd78gv3wpqhdxpsa78za1gm")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/checks-api/1.7.2/checks-api.hpi")
     (sha256 (base32 "02wdkb69pyik96hqdfvgz72g65ksi1cfmvajc3yrahq5kh0fryzg")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/cloudbees-bitbucket-branch-source/2.9.10/cloudbees-bitbucket-branch-source.hpi")
     (sha256 (base32 "11b999j929c4jyxx3g7h1s013mkh7dd62llr7rba75cwpv7vlzg0")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/cloudbees-folder/6.16/cloudbees-folder.hpi")
     (sha256 (base32 "0c34idh9bwl0agh2b2mb74h8xzyplfn6db5dhg7dqg33cag00msr")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/command-launcher/1.6/command-launcher.hpi")
     (sha256 (base32 "1dq7mgc58aydwr5b3y410lv7b7zj1wsflw3nkxfancap6ywz1ikl")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/conditional-buildstep/1.4.1/conditional-buildstep.hpi")
     (sha256 (base32 "0k2sa88z2fvn1kdjhv41mf8lv7jj69myzvj4mhm0dwvajyg6gpgb")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/config-file-provider/3.8.1/config-file-provider.hpi")
     (sha256 (base32 "1790hjyja70cj6wxa7g8111abx644wfnzm3rpzbqaxr5gnc42yjv")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/configuration-as-code/1.51/configuration-as-code.hpi")
     (sha256 (base32 "0c9i3x8csh6yz8y4c0j89pm3xsa7jd9qk2jkhgf64sv9n6wkr3zq")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/credentials/2.5/credentials.hpi")
     (sha256 (base32 "0a6j6azan0n5jz8dr399c100qzwzb01h66s9b6mh1g9wwdai0vpf")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/credentials-binding/1.27/credentials-binding.hpi")
     (sha256 (base32 "1qypzi3ixa4mn5k44hxka3nzwnmpccr239f7spwpyvdikyjaj2dr")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/display-url-api/2.3.5/display-url-api.hpi")
     (sha256 (base32 "02ajnh789mvwf2aqpggrsc4hh2bzqvfpa2wrz8fg19zsp4h8hhaa")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/docker-commons/1.17/docker-commons.hpi")
     (sha256 (base32 "1x3pkzikmni77xmpng659cq1b7mpn54g9y0ijrmn2q00s24ki07q")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/docker-workflow/1.26/docker-workflow.hpi")
     (sha256 (base32 "0g7xl3y2y988rm0lk6w3hzqvg59y2fd6ramjkv9jiwpw33wrhnyy")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/durable-task/1.39/durable-task.hpi")
     (sha256 (base32 "1376ll49zk6ygv00y7djwfsh4095bxwicr9pgdpg5kgr7mkd7b7y")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/echarts-api/5.1.2-9/echarts-api.hpi")
     (sha256 (base32 "00c9ikv5arpgnv6l367mh2dsxsm0c9rb5in357b59zncnr246vgb")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/favorite/2.3.3/favorite.hpi")
     (sha256 (base32 "0gi9kfxz1ymnjn083ajf4jjlzbi2d09ihyh6yxqa72r6p617zyic")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/font-awesome-api/5.15.3-4/font-awesome-api.hpi")
     (sha256 (base32 "0d6jysc5cqqhz73v67n549vxvjxbp60xj03vc3myn9izdfkhkv9d")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/git/4.8.1/git.hpi")
     (sha256 (base32 "17kng2bkabp5n62b1h6gdwbfss5f24dik6c93z4mhp5l1kzr7gcb")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/git-client/3.9.0/git-client.hpi")
     (sha256 (base32 "11p95vy9lqcyv9k18mvcs0xack7jbva1mlcc931dpkc2za21cwif")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/git-server/1.10/git-server.hpi")
     (sha256 (base32 "1f58hzjxlw464pljahw8n0r4f92yrr4ql72rlzgm54jfssg5n29l")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/github/1.33.1/github.hpi")
     (sha256 (base32 "12xaaygi7qfw9jxfmka5kda6js1wmjrz4yk7fpdiyqa174jfmgr4")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/github-api/1.123/github-api.hpi")
     (sha256 (base32 "07mbcv64866gv00vhsh5s0jsjzmlmi633sdc7pmb3d9kx5pyym5n")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/github-branch-source/2.11.2/github-branch-source.hpi")
     (sha256 (base32 "0d9ddaq4x3yb9x48c6f0w0w5sh9y7qxbq4lpjbhzzknkrgyv90pm")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/gitlab-api/1.0.6/gitlab-api.hpi")
     (sha256 (base32 "02frggjnraiz3mlsc17jywqv7ll75nsplfv5h93mjyjxwhn59wwd")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/gitlab-branch-source/1.5.8/gitlab-branch-source.hpi")
     (sha256 (base32 "14pmvn4j8vmkpy8071yb6afnynzj0yhm9q077jv4p20nhnx2xd2z")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/gradle/1.37.1/gradle.hpi")
     (sha256 (base32 "1criba83msmx3k3ykbyblaqsqhl68xm7j2rbiysx7pvwf6kd6mz6")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/handlebars/3.0.8/handlebars.hpi")
     (sha256 (base32 "0vcqlijhnklsn9qwb6xzwn2wpc5j5fbyalh5wjrjvkcbr121mqpp")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/handy-uri-templates-2-api/2.1.8-1.0/handy-uri-templates-2-api.hpi")
     (sha256 (base32 "1s3g3mnzphxi1pk7y8m3w83wmvvkbq5fxs7i1b2cg5gn16c8sryd")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/hashicorp-vault-plugin/3.8.0/hashicorp-vault-plugin.hpi")
     (sha256 (base32 "0ks2qv4r7xgi8iyp1xsah8x9q64fikd5cgkpqkrdmdlvyclxvr75")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/htmlpublisher/1.25/htmlpublisher.hpi")
     (sha256 (base32 "09ykl03xlmdddar0x5qkdmbwx56azxvvcgzmdszjwr6imjd3p5vb")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/jackson2-api/2.12.4/jackson2-api.hpi")
     (sha256 (base32 "00lnc3zmw8v5vg9a353ixyj14k9cdfzic27pi98w00kg74hib5pm")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/javadoc/1.6/javadoc.hpi")
     (sha256 (base32 "1jdvwqfmb5xpx6z17fl9rkxmsrh5yc9l84b235w6ih4yxld35wm5")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/jenkins-design-language/1.24.8/jenkins-design-language.hpi")
     (sha256 (base32 "0bwzz7i2habay68sh3hmfs8ywk8d37f61py7by7m7r6304qbrv8b")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/jira/3.5/jira.hpi")
     (sha256 (base32 "0nlsysjxb3l46ngm85ayck2mh516yb8l29mx5darm12xvcj4kpyy")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/jjwt-api/0.11.2-9.c8b45b8bb173/jjwt-api.hpi")
     (sha256 (base32 "1lrz4m0536rjrm8jhx6b6c2d2097llbwsi1i3hcj36130ipgjldh")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/job-dsl/1.77/job-dsl.hpi")
     (sha256 (base32 "08mj4x2n5q66lkpax1rvflqky4s1xq6bm5fnpqy3wnfhfmfs6wbh")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/jquery3-api/3.6.0-2/jquery3-api.hpi")
     (sha256 (base32 "0knyf18ncnj3brpvg33nc14xcy0208x9axigay618q0ic42m7dq9")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/jsch/0.1.55.2/jsch.hpi")
     (sha256 (base32 "0i7w4f4iv2ji6swak7b9720414c8dj32zflqmpk0md1ywkw4piyd")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/junit/1.52/junit.hpi")
     (sha256 (base32 "02pw7nw834nj01pn01dvhqi754z4rz8h80jvimx43gd68dlgl6kd")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/lockable-resources/2.11/lockable-resources.hpi")
     (sha256 (base32 "0ch527i8qr1hbp1k4g4pmapih8kcxw40mk4f7nd9awrfx72qq4fy")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/mailer/1.34/mailer.hpi")
     (sha256 (base32 "0a8x55ppmvbqpgkpd11zfzi5cgrmj1rzagqz8y45c58r1m09i2x3")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/managed-scripts/1.5.4/managed-scripts.hpi")
     (sha256 (base32 "0yqiq1w9f3bw5xsv20s1ih5q6jn7pbkxlpa5h7iqa8kcrb1rkz2k")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/mapdb-api/1.0.9.0/mapdb-api.hpi")
     (sha256 (base32 "0wgqyc0fym767m9yhxyhjs07x9sy1i1ipw2bqkwqf7zj9jii2b07")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/matrix-project/1.19/matrix-project.hpi")
     (sha256 (base32 "17x4kr29g18za19fiymfyr2m18r3jaigrkh1060vnmwc9yl599n2")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/maven-plugin/3.12/maven-plugin.hpi")
     (sha256 (base32 "06my9y4r4xnnf2zlrspbn4f0d7qipz3gimh1lmk5v19h2zgcjqsl")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/metrics/4.0.2.8/metrics.hpi")
     (sha256 (base32 "0d8q536qzawj96hwkdhh6w72yzhy8qx14igiknxyl2j79w06j952")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/momentjs/1.1.1/momentjs.hpi")
     (sha256 (base32 "0j28ib5mn5yf9n2rnhdkp7yvzgy02cgxxiqdj0ggfmgz9hk2sg6a")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/multiple-scms/0.8/multiple-scms.hpi")
     (sha256 (base32 "04r8ynb2kgdgzmssb0qkp0xxhjzagmbj9jqs6b91jpsya1xg5ppg")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/nexus-jenkins-plugin/3.11.20210811-095455.fdf8fec/nexus-jenkins-plugin.hpi")
     (sha256 (base32 "1ppykw267rjqmayxwzsxh00258pq1l8jyrgp03b13mrab45d9six")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/node-iterator-api/1.5.0/node-iterator-api.hpi")
     (sha256 (base32 "0ffznzgaaf4h0bgksfd2xd1b44dp4fc4lgrzjqlnw23s33r086sk")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/okhttp-api/3.14.9/okhttp-api.hpi")
     (sha256 (base32 "1z8js22p9rvac2vmxwh80ly0ksp2jfhawpz25r4z0h6859j1nr6z")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/p4/1.11.5/p4.hpi")
     (sha256 (base32 "03zd91c2l259kljln31jnh4h8ykjf3z5wyy6lvmniwa9jv4q2jkj")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/parameterized-trigger/2.41/parameterized-trigger.hpi")
     (sha256 (base32 "0z5kckd0dzjvqcvmsl8ilfzpr02fsdpkjwm0ipdvg0jc30vzk9i9")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/pipeline-build-step/2.15/pipeline-build-step.hpi")
     (sha256 (base32 "03iaw83pjkzvrybniykrac41kbzpf2bqvbsvpdkal19k4bbkik9n")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/pipeline-graph-analysis/1.11/pipeline-graph-analysis.hpi")
     (sha256 (base32 "1kk4pydcxz3i5rr75nflmyn2i20sv41s9s2gywnq6qq5ppx6b29a")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/pipeline-input-step/2.12/pipeline-input-step.hpi")
     (sha256 (base32 "1b1v193szz9knjyfa826lr6593m66cyw92paaz6lvglxq11wk0pb")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/pipeline-milestone-step/1.3.2/pipeline-milestone-step.hpi")
     (sha256 (base32 "147m36p6l1ypzdb2sj97i1phf1ghyb1jwysxhy4305z04i80a4dc")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/pipeline-model-api/1.9.1/pipeline-model-api.hpi")
     (sha256 (base32 "16ilx0xax2vkkdfjyb8973c24gqbnn9w8k6y50ls1krxhhyd0lm3")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/pipeline-model-definition/1.9.1/pipeline-model-definition.hpi")
     (sha256 (base32 "0d2qn5f1czk2d80cg7pfchnq76rlrgphk26pfln4lwgldhaq81is")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/pipeline-model-extensions/1.9.1/pipeline-model-extensions.hpi")
     (sha256 (base32 "06sizwif9vx4djgnkqn922zrwc4l4yqiigihfn6j123mxa954msb")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/pipeline-rest-api/2.19/pipeline-rest-api.hpi")
     (sha256 (base32 "129y3hdrs3z5bgv7r8kkv71kl673pljdn3y0s2yafa0q02rx924k")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/pipeline-stage-step/2.5/pipeline-stage-step.hpi")
     (sha256 (base32 "08v5djzlfdxa81vlw9jd9k7hlkw15ygq7ywfjyxa0smx9lbn53x9")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/pipeline-stage-tags-metadata/1.9.1/pipeline-stage-tags-metadata.hpi")
     (sha256 (base32 "1jq8i3agpagv1rbi8bkr02lb8vh1n043wq43yqwgar2j0rlwxijp")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/pipeline-stage-view/2.19/pipeline-stage-view.hpi")
     (sha256 (base32 "1klpabjdyvarvp0sh83d11l1gc8kawk8qnz3wiczks1kk8cm9vjx")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/pipeline-utility-steps/2.8.0/pipeline-utility-steps.hpi")
     (sha256 (base32 "1vqzq1ldzgly0i9fj2lljp0dgagxgx48wr3i07kphjpmjwdyzxy0")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/plain-credentials/1.7/plain-credentials.hpi")
     (sha256 (base32 "1a1v2r6q1gidd6sg20ybq7ar7abyvwgm6nv9cgvcrnlpndkcm0js")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/plugin-util-api/2.4.0/plugin-util-api.hpi")
     (sha256 (base32 "0lsvknji8nlr89cv81j49qh6i85vymnij48m7vdvimy2pjxnvr4g")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/popper-api/1.16.1-2/popper-api.hpi")
     (sha256 (base32 "1wp3cnmvcl0vy7v14w18i6q2g2h3rhpdlxk3x0j2fjd3kwkzdfy5")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/popper2-api/2.9.3-1/popper2-api.hpi")
     (sha256 (base32 "0lg58b52lgajf3048qgrflgw1r071qx1cpjpa39p3jsvb62aw5sh")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/project-inheritance/21.04.03/project-inheritance.hpi")
     (sha256 (base32 "1xzc422xbsmi3pllq99ry4cidnn628fvc7m9z4cvgklnl3919ry7")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/promoted-builds/3.10/promoted-builds.hpi")
     (sha256 (base32 "1fb7v4ribm999c0lfdbf9bc51asx52p19v3bkw09xk93v2lg3i8w")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/pubsub-light/1.16/pubsub-light.hpi")
     (sha256 (base32 "1l8r28hrdcd48xn9iq1904qca6p8kvx904m8gbviifwjmjd9qi0z")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/rebuild/1.32/rebuild.hpi")
     (sha256 (base32 "06l4r3raawy7whcjxkyh3164f1hxf202zky4gc9zgh0hj4g0kwa4")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/resource-disposer/0.16/resource-disposer.hpi")
     (sha256 (base32 "1aps8b4d1d3a9k7rmv2n2dc7rcihw9mxhpdf1hmy37br0604r4px")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/run-condition/1.5/run-condition.hpi")
     (sha256 (base32 "1jirnf05j1dqcv1d3khdfx4hxvhyhf8y3xsvbgj00v37jrqlvnby")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/scm-api/2.6.5/scm-api.hpi")
     (sha256 (base32 "0h68w81yh81pf13v51kgxvvlifbnkl3wxniqqpzri425wrhw9b90")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/script-security/1.78/script-security.hpi")
     (sha256 (base32 "1nx2ad3dxs9hn8g86i4a6csfkndzid2xp0jmhpbpckn4m79a4m0j")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/slack/2.48/slack.hpi")
     (sha256 (base32 "1qym6zqs4b034vfmdb8053bf1a69z0bgcszqxrhr8z5l5fr9qvr2")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/snakeyaml-api/1.29.1/snakeyaml-api.hpi")
     (sha256 (base32 "15amdsdscdlpmj7pwwwy3xmsvpp20ns8hfb6961yi92pwvqk92wz")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/sse-gateway/1.24/sse-gateway.hpi")
     (sha256 (base32 "076mr3134cqazfnd5b69m9n3h5p05kij4aic0ni8j4x4m340k4lf")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/ssh-credentials/1.19/ssh-credentials.hpi")
     (sha256 (base32 "05gd962yv3l6zb5yiljq4q20qgwpbdkmvc93x98ki7b6r153rjbd")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/ssh-slaves/1.32.0/ssh-slaves.hpi")
     (sha256 (base32 "0vvj73bfpaq2yxi5y4n7hn65x7gnrclyvj22agn0x7wqfmbsw32d")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/sshd/3.1.0/sshd.hpi")
     (sha256 (base32 "0547ym8zxka6i7r4z7cc658j60r14ydixh1pd3anmyzdcir4hk4q")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/structs/1.23/structs.hpi")
     (sha256 (base32 "12iksvs2shgyjq82r49m38wyg3bky4hi7a11d51qgy779i5x8lqd")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/subversion/2.14.4/subversion.hpi")
     (sha256 (base32 "05qlcmg2081l2p43m2ghrfcb26r8j4yd6yj7filal57jjj3l0a66")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/support-core/2.74/support-core.hpi")
     (sha256 (base32 "13pw3b7cmz3xvzq7d2rcg9x7dvqvi8hwx6rxq9n0a6jyyhwx4md2")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/token-macro/266.v44a80cf277fd/token-macro.hpi")
     (sha256 (base32 "0ksbkhd6ly5pikvapz9871d4wv9a5i7vhabafx5z7cxqy122b19p")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/trilead-api/1.0.13/trilead-api.hpi")
     (sha256 (base32 "0mz407qnzncx9dqdm2n5pyzx6p46bnby9d420fawdkwkwhw6id32")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/variant/1.4/variant.hpi")
     (sha256 (base32 "1wv7bdh788868h2qf1dz2qm7p7w42vzb160q8za3s8m9k8j8aaac")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/vsphere-cloud/2.25/vsphere-cloud.hpi")
     (sha256 (base32 "1mqx13dlasrd8gh70539cxzbfr3p2jziv7rmp5vxb9m3dbk73qrl")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/workflow-aggregator/2.6/workflow-aggregator.hpi")
     (sha256 (base32 "1xjbw85skf93cazvc1m32wh8gxlcrawxmlgj7s2qkrg6fxr4dk5x")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/workflow-api/2.46/workflow-api.hpi")
     (sha256 (base32 "01xm3gsv2qdc6jcj3aj9200bhb120d9xhnrz26ai59kc6qs8kvsh")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/workflow-basic-steps/2.24/workflow-basic-steps.hpi")
     (sha256 (base32 "01h5a6dq57n92m5f8bp08bscsh29q00pk67zlvkkpa68q3rrmcrw")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/workflow-cps/2.93/workflow-cps.hpi")
     (sha256 (base32 "0iivn2gygxx1k3p24kqmqs66qyfx784lkvlkygvhgzs4hkdpylw6")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/workflow-cps-global-lib/2.21/workflow-cps-global-lib.hpi")
     (sha256 (base32 "0a1gsdjfnx4gqd4396q07h0a903kqdg9438rd95hbbfjb05yzqik")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/workflow-durable-task-step/2.39/workflow-durable-task-step.hpi")
     (sha256 (base32 "0mcbbmbahh0h2mdd1cm5frsvfcmzhiyqvwbpvd80ykhfmr3aiky2")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/workflow-job/2.41/workflow-job.hpi")
     (sha256 (base32 "1lg3dzpp0kqlc75l2vap59xdbvr2rzskgf8zlfjvp13bzd7x4y7g")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/workflow-multibranch/2.26/workflow-multibranch.hpi")
     (sha256 (base32 "040420qh0c88p1bjv15q3nyb9x32jakfim0w5dal31zy09lpzmn2")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/workflow-scm-step/2.13/workflow-scm-step.hpi")
     (sha256 (base32 "1r6514i19b4nc5q5yrvka5sj0g9csisclpqffq2739z3jn3p55va")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/workflow-step-api/2.24/workflow-step-api.hpi")
     (sha256 (base32 "0rj2wgfnb7jd7gnvl3v0ag4ypxbx6g8nfp4v0s1j9394vkmjkmp6")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/workflow-support/3.8/workflow-support.hpi")
     (sha256 (base32 "0f1ir6793h4l86sbyqbiw8iz4l25lfxr71wdvyi7flm0i35nk1z8")))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/ws-cleanup/0.39/ws-cleanup.hpi")
     (sha256 (base32 "1wz42q05zc02qwskm4skk7g53lhn33ga4cdgbvhjabzfi82z1j3v")))))
