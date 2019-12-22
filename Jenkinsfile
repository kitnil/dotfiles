pipeline {
    agent {
        label "master"
    }
    stages {
        stage("git clone dotfiles") {
            steps {
                parallelGitClone url: "https://cgit.duckdns.org/git/wigust/dotfiles", nodeLabels: ["guix"], dir: "/home/oleg/src/dotfiles"
            }
        }
    }
}
