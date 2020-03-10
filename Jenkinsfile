String HOME_DIR = "/home/oleg"

List<String> node_labels = ["guix", "guix nixbld", "guix vm"]

pipeline {
    agent {
        label "master"
    }
    stages {
        stage("Invoking git clone") {
            steps {
                parallelGitClone url: "https://cgit.duckdns.org/git/wigust/chezmoi",
                nodeLabels: node_labels,
                dir: "$HOME_DIR/.local/share/chezmoi"
            }
        }
   }
    post {
        always {
            sendNotifications currentBuild.result
        }
    }
}
