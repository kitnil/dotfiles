String HOME_DIR = "/home/oleg"

List<String> node_labels = ["guix", "guix nixbld", "guix vm"]

pipeline {
    agent {
        label "master"
    }
    parameters {
        booleanParam(name: 'CHEZMOI_APPLY',
                     defaultValue: triggeredBy('UpstreamCause') == true ? false : true,
                     description: 'Invoke chezmoi apply')
    }
    stages {
        stage("Invoking git clone") {
            steps {
                parallelGitClone url: "https://cgit.duckdns.org/git/wigust/chezmoi",
                nodeLabels: node_labels,
                dir: "$HOME_DIR/.local/share/chezmoi"
            }
        }
        stage("Invoke chezmoi") {
            steps {
                script {
                    apply = params.CHEZMOI_APPLY ? "chezmoi apply" : "true"
                    parallelSh cmd: "chezmoi diff; $apply", nodeLabels: node_labels
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
