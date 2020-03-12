String HOME_DIR = "/home/oleg"

List<String> node_labels = ["guix", "guix nixbld", "guix vm"]

def NIX_ENV_COMMAND =
    [([".", "$HOME_DIR/.nix-profile/etc/profile.d/nix.sh"].join(" ")),
     (["NIX_PATH=nixpkgs=$HOME_DIR/.nix-defexpr/channels/nixos-unstable",
       "nix-env", "--install", "'.*'", "-f", "$HOME_DIR/manifest.nix"].join(" "))].join("; ")

String GIT_PULL_REMOTE = "upstream"
String GUIX_PULL_BRANCH = "wip-local"
String GUIX_CHANNELS_FILE = "$HOME_DIR/channels.scm"
List<String> GUIX_PULL_ARGS = [
    "--branch=${GUIX_PULL_BRANCH}",
    "--channels=${GUIX_CHANNELS_FILE}"
]
String GUIX_PULL_COMMAND = "guix pull ${GUIX_PULL_ARGS.join(' ')}"

pipeline {
    agent {
        label "master"
    }
    environment {
        NIXPKGS_CONFIG="${WORKSPACE}/nix/config.nix"
    }
    parameters {
        booleanParam name: 'INVOKE_GUIX_PULL', defaultValue: true,
        description: 'Invoke guix pull'
    }
    stages {
        stage("Invoking build.sh") {
            when { expression { params.INVOKE_GUIX_PULL } }
            steps {
                sh "${WORKSPACE}/build.sh"
            }
        }
        stage("Invoking guix pull") {
            when { expression { params.INVOKE_GUIX_PULL } }
            steps {
                parallelSh cmd: GUIX_PULL_COMMAND, nodeLabels: node_labels
            }
        }
        stage("Invoking guix pull as root") {
            when { expression { params.INVOKE_GUIX_PULL } }
            steps {
                parallelSh cmd: "sudo -i ${GUIX_PULL_COMMAND}",
                nodeLabels: node_labels
            }
        }
        stage("Invoking git clone") {
            steps {
                parallelGitClone url: "https://cgit.duckdns.org/git/wigust/dotfiles",
                nodeLabels: node_labels,
                dir: "$HOME_DIR/src/dotfiles"
            }
        }
        stage("Invoking guix system build") {
            steps {
                parallelSh cmd: "guix system build /etc/config.scm",
                nodeLabels: node_labels
            }
        }
        stage("Invoking guix package") {
            steps {
                parallelSh cmd: "guix environment --manifest=$HOME_DIR/manifest.scm -- sh -c exit",
                nodeLabels: node_labels
            }
        }
        stage("Invoking nix-env") {
            steps {
                parallelSh cmd: NIX_ENV_COMMAND,
                nodeLabels: node_labels
            }
        }
        stage('Trigger jobs') {
            steps {
                catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
                    build job: "../../nix/maintenance/wip-local"
                }
                build job: "../chezmoi/master"
            }
        }
   }
    post {
        always {
            sendNotifications currentBuild.result
        }
    }
}
