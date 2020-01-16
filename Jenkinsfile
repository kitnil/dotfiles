pipeline {
    agent {
        label "master"
    }
    stages {
        stage("Cloning from local Git") {
            steps {
                parallelGitClone url: "https://cgit.duckdns.org/git/wigust/dotfiles",
                nodeLabels: ["guix", "guix nixbld", "guix vm"],
                dir: "/home/oleg/src/dotfiles"
            }
        }
        stage("Invoking guix system build") {
            steps {
                parallelSh cmd: "guix system build /etc/config.scm",
                nodeLabels: ["guix", "guix nixbld", "guix vm"]
            }
        }
        stage("Invoking guix package") {
            steps {
                parallelSh cmd: "guix package --manifest=/home/oleg/manifest.scm",
                nodeLabels: ["guix", "guix nixbld", "guix vm"]
            }
        }
    }
    post {
        always {
            sendNotifications currentBuild.result
        }
    }
}
