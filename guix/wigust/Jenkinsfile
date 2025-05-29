pipeline {
    agent { label "master" }
    stages {
        stage("git clone guix-wigust") {
            steps {
                parallelGitClone (
                    url: "https://cgit.duckdns.org/git/guix/guix-wigust",
                    nodeLabels: ["guix"],
                    dir: "$Constants.homeDir/src/guix-wigust"
                )
            }
        }
    }
}
