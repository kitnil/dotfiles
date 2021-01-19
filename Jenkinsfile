pipeline {
    agent { label "guixsd" }
    options {
        disableConcurrentBuilds()
        timeout(time: 1, unit: "HOURS")
    }
    stages {
        stage("Deploy") {
            agent { label "master" }
            steps {
                parallelCall (
                    nodeLabels: ["guix"],
                    procedure: { nodeLabels ->
                        gitFetch (
                            url: library('jenkins-wi-shared-library').Constants.gitDotfilesUrl,
                            dir: library('jenkins-wi-shared-library').Constants.homeDir + "/.local/share/chezmoi"
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
