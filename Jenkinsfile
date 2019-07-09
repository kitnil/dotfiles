pipeline {
    agent {
        label 'cuirass'
    }
    stages {
        stage('pull') {
            steps {
                dir('dotfiles') {
                    git url: 'https://gitlab.wugi.info/wigust/dotfiles.git'
                }
                sh "guix pull --channel=guix,https://gitlab.wugi.info/guix/guix.git,branch=wip --channel=guix-wigust,https://gitlab.wugi.info/guix/guix-wigust.git"
            }
        }
        stage('build') {
            steps {
                dir('dotfiles') {
                    sh 'guix environment --manifest=fiore/manifests/natsu-manifest.scm -- sh -c exit'
                    sh 'guix system build guixsd/config.scm'
                }
            }
        }
    }
    // https://github.com/jenkinsci/gitlab-plugin/issues/462
    post {
	success {
	    echo 'posting success to GitLab'
            updateGitlabCommitStatus(name: 'jenkins-build', state: 'success')

            echo 'posting success to Alerta'
            sh 'ALERTA_KEY="QbWN0zj4R-XpsFYK53qXC_ytnZOYHo0IZVQbVDF7" /home/jenkins/bin/guile-alerta --result=success --job=fiore'
  	}
	failure {
	    echo 'postinng failure to GitLab'
            updateGitlabCommitStatus(name: 'jenkins-build', state: 'failed')

            echo 'posting failure to Alerta'
            sh 'ALERTA_KEY="QbWN0zj4R-XpsFYK53qXC_ytnZOYHo0IZVQbVDF7" /home/jenkins/bin/guile-alerta --result=fail --job=fiore'
  	}
    }
}
