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
                dir("dotfiles") {
                    sh "./build.sh"
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
