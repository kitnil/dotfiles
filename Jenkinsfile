pipeline {
    agent {
        label "master"
    }
    stages {
        stage("git clone guix-wigust-services") {
            steps {
                parallelGitClone url: "https://cgit.duckdns.org/git/guix/guix-wigust-services", nodeLabels: ["guix"], dir: "/home/oleg/src/guix-wigust-services"
            }
        }
    }
}
