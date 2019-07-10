pipeline {
    agent {
        label 'guixsd'
    }
    parameters {
        string(name: 'COMMIT', defaultValue: '', description: 'Guix Git commit hash')
    }
    stages {
        stage('pull') {
            steps {
                sh "~/.config/guix/current/bin/guix pull --substitute-urls='https://ci.guix.info' --profile=guix-jenkins --commit=${COMMIT}"
                sh "./guix-jenkins/bin/guix describe"
            }
        }
        stage('manifest') {
            steps {
                sh "./guix-jenkins/bin/guix environment --substitute-urls='https://ci.guix.info' --manifest=fiore/manifests/guix-collection.scm -- sh -c exit"
            }
        }
        stage('system') {
            steps {
                sh "./guix-jenkins/bin/guix system build --load-path=fiore/modules --substitute-urls='https://ci.guix.info' guixsd/config.scm"
            }
        }
    }
    post {
        success {
            slackSend color: '#00CC00', iconEmoji: '', message: "Guix pipeline succeed.", username: ''
        }
        failure {
            slackSend color: '#ADD8E6', iconEmoji: '', message: "Guix pipeline failed.", username: ''
        }
    }
}





