def lib = library('jenkins-wi-shared-library')

pipeline {
    agent { label "guixsd" }
    options {
        disableConcurrentBuilds()
        timeout(time: 1, unit: "HOURS")
    }
    stages {
        stage("benchmark") {
            agent { label "guixsd" }
            steps {
                sh "make benchmark"
            }
        }
        stage("deploy") {
            agent { label "master" }
            steps {
                parallelCall (
                    nodeLabels: ["guix"],
                    procedure: { nodeLabels ->
                        gitFetch (
                            url: lib.Constants.gitDotfilesUrl,
                            dir: lib.Constants.homeDir + "/.local/share/chezmoi"
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
