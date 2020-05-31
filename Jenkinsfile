pipeline {
    agent { label "guixsd" }
    options { disableConcurrentBuilds() }
    stages {
        stage("Deploy") {
            agent { label "master" }
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
