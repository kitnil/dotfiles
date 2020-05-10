String HOME_DIR = "/home/oleg"
String MASTER_LABEL = "guixsd"
List<String> node_labels = ["guix", MASTER_LABEL, "guix vm"]

pipeline {
    agent {
        label "master"
    }
    options {
        disableConcurrentBuilds()
    }
    stages {
        stage("Clone dotfiles from origin/master") {
            steps {
                parallelGitClone url: "https://cgit.duckdns.org/git/wigust/dotfiles",
                nodeLabels: (node_labels - MASTER_LABEL),
                dir: "$HOME_DIR/.local/share/chezmoi"
            }
        }
        stage("Build current Guix system") {
            steps {
                parallelSh (cmd: (["guix system build /etc/config.scm",
                                   "guix environment --manifest=/home/oleg/manifest.scm -- sh -c exit"].join("; ")),
                            nodeLabels: node_labels)
            }
        }
        stage("Build Guix things with latest channels") {
            agent { label "guixsd" }
            steps {
                script {
                    sh "guix pull --verbosity=0 --profile=$WORKSPACE/.guix-profile --channels=$WORKSPACE/dotfiles/channels.scm"
                    String guixBinary = "$WORKSPACE/.guix-profile/bin/guix"
                    (sh (script: "ls -1 $WORKSPACE/dotfiles/guixsd/*.scm", returnStdout: true)).trim().split("\n").each { system ->
                        sh (["$guixBinary system build --load-path=$WORKSPACE/dotfiles/fiore/modules $system",
                             (["$guixBinary", "environment", ("--manifest=" + "$WORKSPACE/dotfiles/manifests/" + system.split("/").last()), "--", "sh", "-c", "exit"].join(" "))].join("; "))
                    }
                }
            }
        }
   }
    post {
        always {
            sendNotifications currentBuild.result
        }
    }
}
