pipeline {
    agent { label "master" }
    options { disableConcurrentBuilds() }
    stages {
        stage("Build") {
            agent { label "guixsd" }
            steps {
                script {
                    String hostname = sh (
                        script: "hostname",
                        returnStdout: true
                    ).trim()
                    sh (["guix", "pull", "--verbosity=0",
                         "--profile=$WORKSPACE/.guix-profile",
                         "--channels=$WORKSPACE/dotfiles/channels.scm"
                        ].join(" "))
                    String guixBinary = "$WORKSPACE/.guix-profile/bin/guix"
                    (sh (script: "ls -1 $WORKSPACE/dotfiles/guixsd/*.scm",
                         returnStdout: true)).trim().split("\n").each { system ->
                        sh (["$guixBinary system build --load-path=$WORKSPACE/dotfiles/fiore/modules $system",
                             (["$guixBinary", "environment",
                               ("--manifest=" + "$WORKSPACE/dotfiles/manifests/"
                                + system.split("/").last()),
                               "--", "sh", "-c", "exit"].join(" "))].join("; "))
                    }
                }
            }
        }
        stage("Deploy") {
            steps {
                parallelCall (
                    nodeLabels: ["guix"],
                    procedure: { nodeLabels ->
                        gitFetch (
                            url: Constants.gitGuixUrl,
                            dir: "$Constants.homeDir/.local/share/chezmoi"
                        )
                    }
                )
            }
        }
    }
    post {
        always {
            sendNotifications currentBuild.result
        }
    }
}
