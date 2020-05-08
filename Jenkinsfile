String HOME_DIR = "/home/oleg"
String MASTER_LABEL = "guix nixbld"
List<String> node_labels = ["guix", MASTER_LABEL, "guix vm"]

pipeline {
    agent {
        label "master"
    }
    options {
        disableConcurrentBuilds()
    }
    parameters {
        booleanParam(name: 'WITH_LATEST_GUIX',
                     defaultValue: false,
                     description: 'Build Guix things with latest channels')
    }
    stages {
        stage("Clone dotfiles from origin/master") {
            steps {
                parallelGitClone url: "https://cgit.duckdns.org/git/wigust/dotfiles",
                nodeLabels: (node_labels - MASTER_LABEL),
                dir: "$HOME_DIR/.local/share/chezmoi"
            }
        }
        stage("Build Guix things with latest channels") {
            when { anyOf {
                    triggeredBy('TimerTrigger')
                    expression { params.WITH_LATEST_GUIX }
                }
            }
            steps {
                dir("dotfiles") {
                    sh "./build.sh"
                }
            }
        }
        stage("Build current Guix system") {
            steps {
                parallelSh (cmd: (["guix system build /etc/config.scm",
                                   "guix environment --manifest=/home/oleg/manifest.scm -- sh -c exit"].join("; ")),
                            nodeLabels: node_labels)
            }
        }
   }
    post {
        always {
            sendNotifications currentBuild.result
        }
    }
}
