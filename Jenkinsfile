pipeline {
    agent {
        label 'guixsd'
    }
    stages {
        stage('pull') {
            input {
                message "Commit"
                parameters {
                    string(name: 'GUIX_COMMIT', defaultValue: 'fb2abbd6f5abac8ae0fec594594dd54ff7e3f4db', description: 'Guix Git commit hash')
                }
            }
            steps {
                sh "~/.config/guix/current/bin/guix pull --substitute-urls='https://ci.guix.info' --profile=guix-jenkins --commit=${GUIX_COMMIT}"
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





