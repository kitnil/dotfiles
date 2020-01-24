def NODES = ["guix", "guix nixbld", "guix vm"]

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
                nodeLabels: NODES
            }
        }
        stage("Invoking guix package") {
            steps {
                parallelSh cmd: "guix package --manifest=/home/oleg/manifest.scm",
                nodeLabels: NODES
            }
        }
        stage("Invoking nix-env") {
            steps {
                parallelSh cmd: "NIX_PATH=nixpkgs=$HOME/.nix-defexpr/channels/nixos-unstable nix-env --install '.*' -f $HOME/manifest.nix",
                nodeLabels: NODES
            }
        }
    }
    post {
        always {
            sendNotifications currentBuild.result
        }
    }
}
