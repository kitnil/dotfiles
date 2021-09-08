pipeline {
    agent { label "guixsd" }
    options {
        disableConcurrentBuilds()
        timeout(time: 1, unit: "HOURS")
    }
    stages {
        stage("build") {
            agent { label "guixsd" }
            steps {
                sh ":"
            }
        }
    }
}
