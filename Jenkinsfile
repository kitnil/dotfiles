@Library('jenkins-wi-shared-library') _

pipeline {
    agent {
        label 'guixsd'
    }
    parameters {
        string(name: 'GUIX_COMMIT',
               defaultValue: '878a6baa4c705f4d551b60c5aa254246e0abc922',
               description: 'Guix Git commit hash')
    }
    stages {
        stage('build') {
            steps {
                sendNotifications 'STARTED'
                sh """~/.config/guix/current/bin/guix pull \
 --substitute-urls='https://ci.guix.info' \
 --profile=guix-jenkins \
 --commit=${GUIX_COMMIT}"""
                sh "./guix-jenkins/bin/guix describe"
                sh """./guix-jenkins/bin/guix environment \
 --substitute-urls='https://ci.guix.info' \
 --manifest=fiore/manifests/guix-collection.scm \
 -- sh -c exit"""
                sh """./guix-jenkins/bin/guix system build \
 --load-path=fiore/modules \
 --substitute-urls='https://ci.guix.info' \
 guixsd/config.scm"""
            }
        }
    }
    post {
        always {
            sendNotifications currentBuild.result
        }
    }
}





