(define-module (jenkins plugins)
  #:use-module (guix download)
  #:use-module (guix packages))

(define-public %jenkins-base-plugins
  (list
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/ace-editor/1.1/ace-editor.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/ansicolor/1.0.0/ansicolor.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/apache-httpcomponents-client-4-api/4.5.13-1.0/apache-httpcomponents-client-4-api.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/async-http-client/1.9.40.0/async-http-client.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/authentication-tokens/1.4/authentication-tokens.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean/1.24.8/blueocean.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-autofavorite/1.2.4/blueocean-autofavorite.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-bitbucket-pipeline/1.24.8/blueocean-bitbucket-pipeline.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-commons/1.24.8/blueocean-commons.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-config/1.24.8/blueocean-config.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-core-js/1.24.8/blueocean-core-js.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-dashboard/1.24.8/blueocean-dashboard.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-display-url/2.4.1/blueocean-display-url.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-events/1.24.8/blueocean-events.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-executor-info/1.24.8/blueocean-executor-info.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-git-pipeline/1.24.8/blueocean-git-pipeline.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-github-pipeline/1.24.8/blueocean-github-pipeline.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-i18n/1.24.8/blueocean-i18n.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-jira/1.24.8/blueocean-jira.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-jwt/1.24.8/blueocean-jwt.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-personalization/1.24.8/blueocean-personalization.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-pipeline-api-impl/1.24.8/blueocean-pipeline-api-impl.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-pipeline-editor/1.24.8/blueocean-pipeline-editor.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-pipeline-scm-api/1.24.8/blueocean-pipeline-scm-api.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-rest/1.24.8/blueocean-rest.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-rest-impl/1.24.8/blueocean-rest-impl.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/blueocean-web/1.24.8/blueocean-web.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/bootstrap4-api/4.6.0-3/bootstrap4-api.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/bootstrap5-api/5.1.0-1/bootstrap5-api.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/branch-api/2.6.5/branch-api.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/caffeine-api/2.9.2-29.v717aac953ff3/caffeine-api.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/checks-api/1.7.2/checks-api.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/cloudbees-bitbucket-branch-source/2.9.10/cloudbees-bitbucket-branch-source.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/cloudbees-folder/6.16/cloudbees-folder.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/command-launcher/1.6/command-launcher.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/conditional-buildstep/1.4.1/conditional-buildstep.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/config-file-provider/3.8.1/config-file-provider.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/configuration-as-code/1.51/configuration-as-code.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/credentials/2.5/credentials.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/credentials-binding/1.27/credentials-binding.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/display-url-api/2.3.5/display-url-api.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/docker-commons/1.17/docker-commons.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/docker-workflow/1.26/docker-workflow.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/durable-task/1.39/durable-task.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/echarts-api/5.1.2-9/echarts-api.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/favorite/2.3.3/favorite.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/font-awesome-api/5.15.3-4/font-awesome-api.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/git/4.8.1/git.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/git-client/3.9.0/git-client.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/git-server/1.10/git-server.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/github/1.33.1/github.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/github-api/1.123/github-api.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/github-branch-source/2.11.2/github-branch-source.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/gitlab-api/1.0.6/gitlab-api.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/gitlab-branch-source/1.5.8/gitlab-branch-source.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/gradle/1.37.1/gradle.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/handlebars/3.0.8/handlebars.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/handy-uri-templates-2-api/2.1.8-1.0/handy-uri-templates-2-api.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/hashicorp-vault-plugin/3.8.0/hashicorp-vault-plugin.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/htmlpublisher/1.25/htmlpublisher.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/jackson2-api/2.12.4/jackson2-api.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/javadoc/1.6/javadoc.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/jenkins-design-language/1.24.8/jenkins-design-language.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/jira/3.5/jira.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/jjwt-api/0.11.2-9.c8b45b8bb173/jjwt-api.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/job-dsl/1.77/job-dsl.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/jquery3-api/3.6.0-2/jquery3-api.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/jsch/0.1.55.2/jsch.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/junit/1.52/junit.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/lockable-resources/2.11/lockable-resources.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/mailer/1.34/mailer.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/managed-scripts/1.5.4/managed-scripts.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/mapdb-api/1.0.9.0/mapdb-api.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/matrix-project/1.19/matrix-project.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/maven-plugin/3.12/maven-plugin.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/metrics/4.0.2.8/metrics.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/momentjs/1.1.1/momentjs.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/multiple-scms/0.8/multiple-scms.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/nexus-jenkins-plugin/3.11.20210811-095455.fdf8fec/nexus-jenkins-plugin.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/node-iterator-api/1.5.0/node-iterator-api.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/okhttp-api/3.14.9/okhttp-api.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/p4/1.11.5/p4.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/parameterized-trigger/2.41/parameterized-trigger.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/pipeline-build-step/2.15/pipeline-build-step.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/pipeline-graph-analysis/1.11/pipeline-graph-analysis.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/pipeline-input-step/2.12/pipeline-input-step.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/pipeline-milestone-step/1.3.2/pipeline-milestone-step.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/pipeline-model-api/1.9.1/pipeline-model-api.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/pipeline-model-definition/1.9.1/pipeline-model-definition.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/pipeline-model-extensions/1.9.1/pipeline-model-extensions.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/pipeline-rest-api/2.19/pipeline-rest-api.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/pipeline-stage-step/2.5/pipeline-stage-step.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/pipeline-stage-tags-metadata/1.9.1/pipeline-stage-tags-metadata.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/pipeline-stage-view/2.19/pipeline-stage-view.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/pipeline-utility-steps/2.8.0/pipeline-utility-steps.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/plain-credentials/1.7/plain-credentials.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/plugin-util-api/2.4.0/plugin-util-api.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/popper-api/1.16.1-2/popper-api.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/popper2-api/2.9.3-1/popper2-api.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/project-inheritance/21.04.03/project-inheritance.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/promoted-builds/3.10/promoted-builds.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/pubsub-light/1.16/pubsub-light.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/rebuild/1.32/rebuild.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/resource-disposer/0.16/resource-disposer.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/run-condition/1.5/run-condition.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/scm-api/2.6.5/scm-api.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/script-security/1.78/script-security.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/slack/2.48/slack.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/snakeyaml-api/1.29.1/snakeyaml-api.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/sse-gateway/1.24/sse-gateway.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/ssh-credentials/1.19/ssh-credentials.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/ssh-slaves/1.32.0/ssh-slaves.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/sshd/3.1.0/sshd.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/structs/1.23/structs.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/subversion/2.14.4/subversion.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/support-core/2.74/support-core.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/token-macro/266.v44a80cf277fd/token-macro.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/trilead-api/1.0.13/trilead-api.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/variant/1.4/variant.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/vsphere-cloud/2.25/vsphere-cloud.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/workflow-aggregator/2.6/workflow-aggregator.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/workflow-api/2.46/workflow-api.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/workflow-basic-steps/2.24/workflow-basic-steps.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/workflow-cps/2.93/workflow-cps.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/workflow-cps-global-lib/2.21/workflow-cps-global-lib.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/workflow-durable-task-step/2.39/workflow-durable-task-step.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/workflow-job/2.41/workflow-job.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/workflow-multibranch/2.26/workflow-multibranch.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/workflow-scm-step/2.13/workflow-scm-step.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/workflow-step-api/2.24/workflow-step-api.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/workflow-support/3.8/workflow-support.hpi")
     (sha256 #f))
   (origin
     (method url-fetch)
     (uri "https://updates.jenkins-ci.org/download/plugins/ws-cleanup/0.39/ws-cleanup.hpi")
     (sha256 #f))))
