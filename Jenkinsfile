def NODES = ["guix", "guix nixbld", "guix vm"]

def NIX_ENV_COMMAND =
    [([".", "/home/oleg/.nix-profile/etc/profile.d/nix.sh"].join(" ")),
     (["NIX_PATH=nixpkgs=/home/oleg/.nix-defexpr/channels/nixos-unstable",
       "nix-env", "--install", "'.*'", "-f", "/home/oleg/manifest.nix"].join(" "))].join("; ")

pipeline {
    agent {
        label "master"
    }
    environment {
        NIXPKGS_CONFIG="${WORKSPACE}/nix/config.nix"
    }
    stages {
        stage("Cloning from local Git") {
            steps {
                parallelGitClone url: "https://cgit.duckdns.org/git/wigust/dotfiles",
                nodeLabels: NODES,
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
                parallelSh cmd: NIX_ENV_COMMAND,
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
