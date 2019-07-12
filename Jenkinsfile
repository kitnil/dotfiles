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
                // Build all my Guix packages. Continue on failure.
                script {
                    ['cava',
                     'cdrkit-libre',
                     'cli-visualizer',
                     'colormake',
                     'ddclient',
                     'eless',
                     'emacs-academic-phrases',
                     'emacs-add-hooks',
                     'emacs-ansible-doc',
                     'emacs-auto-yasnippet',
                     'emacs-beginend',
                     'emacs-benchmark-init',
                     'emacs-biblio',
                     'emacs-browse-at-remote',
                     'emacs-cl-generic',
                     'emacs-closql',
                     'emacs-cmake-mode',
                     'emacs-company-lua',
                     'emacs-company-quickhelp',
                     'emacs-company-restclient',
                     'emacs-crux',
                     'emacs-csv-mode',
                     'emacs-darkroom',
                     'emacs-dashboard',
                     'emacs-datetime',
                     'emacs-default-text-scale',
                     'emacs-dired-hacks',
                     'emacs-dired-rsync',
                     'emacs-discover-my-major',
                     'emacs-docker',
                     'emacs-docker-tramp',
                     'emacs-dockerfile-mode',
                     'emacs-download-region',
                     'emacs-dumb-jump',
                     'emacs-edit-indirect',
                     'emacs-edit-server',
                     'emacs-elisp-refs',
                     'emacs-emacsql',
                     'emacs-emamux',
                     'emacs-emms-player-simple-mpv',
                     'emacs-engine-mode',
                     'emacs-epkg',
                     'emacs-erc-hl-nicks',
                     'emacs-eros',
                     'emacs-esup',
                     'emacs-ewmctrl',
                     'emacs-f3',
                     'emacs-faceup',
                     'emacs-fancy-narrow',
                     'emacs-finalize',
                     'emacs-ggtags',
                     'emacs-git-auto-commit-mode',
                     'emacs-git-messenger',
                     'emacs-gitpatch',
                     'emacs-grep-context',
                     'emacs-helm-bibtex',
                     'emacs-helm-c-yasnippet',
                     'emacs-helm-eww',
                     'emacs-helm-firefox',
                     'emacs-helm-gtags',
                     'emacs-helm-make',
                     'emacs-helm-mode-manager',
                     'emacs-helm-shell-history',
                     'emacs-helpful',
                     'emacs-hierarchy',
                     'emacs-highlight-defined',
                     'emacs-highlight-escape-sequences',
                     'emacs-highlight-numbers',
                     'emacs-highlight-stages',
                     'emacs-highlight-symbol',
                     'emacs-hy-mode',
                     'emacs-ibuffer-projectile',
                     'emacs-ido-vertical-mode',
                     'emacs-interactive-align',
                     'emacs-irfc',
                     'emacs-itail',
                     'emacs-ivy-yasnippet',
                     'emacs-json-mode',
                     'emacs-json-reformat',
                     'emacs-json-snatcher',
                     'emacs-know-your-http-well',
                     'emacs-lacarte',
                     'emacs-let-alist',
                     'emacs-lice-el',
                     'emacs-list-utils',
                     'emacs-load-relative',
                     'emacs-logview',
                     'emacs-loop',
                     'emacs-m-buffer-el',
                     'emacs-macrostep',
                     'emacs-magit-org-todos-el',
                     'emacs-makey',
                     'emacs-mbsync',
                     'emacs-md4rd',
                     'emacs-move-text',
                     'emacs-navi-mode',
                     'emacs-nix-mode',
                     'emacs-nnreddit',
                     'emacs-npm-mode',
                     'emacs-on-screen',
                     'emacs-org-edit-latex',
                     'emacs-org-mind-map',
                     'emacs-org-pomodoro',
                     'emacs-org-ref',
                     'emacs-outorg',
                     'emacs-outshine',
                     'emacs-parent-mode',
                     'emacs-parinfer-mode',
                     'emacs-parsebib',
                     'emacs-pg',
                     'emacs-polymode-ansible',
                     'emacs-pulseaudio-control',
                     'emacs-racket-mode',
                     'emacs-rainbow-blocks',
                     'emacs-rotate-text',
                     'emacs-rpm-spec-mode',
                     'emacs-rsw-elisp',
                     'emacs-scratch-el',
                     'emacs-seq',
                     'emacs-shift-number',
                     'emacs-slime-company',
                     'emacs-sml-mode',
                     'emacs-sourcemap',
                     'emacs-sr-speedbar',
                     'emacs-stickyfunc-enhance',
                     'emacs-stumpwm-mode',
                     'emacs-suggest',
                     'emacs-tiny',
                     'emacs-transmission',
                     'emacs-tree-mode',
                     'emacs-tuareg',
                     'emacs-validate',
                     'emacs-visual-regexp',
                     'emacs-web-beautify',
                     'emacs-which-key',
                     'emacs-wordgen',
                     'epipe',
                     'flameshot',
                     'gource',
                     'keynav',
                     'kodi-cli',
                     'licensecheck',
                     'perl-b-hooks-op-check',
                     'perl-bareword-filehandles',
                     'perl-data-section',
                     'perl-extutils-depends',
                     'perl-indirect',
                     'perl-lexical-sealrequirehints',
                     'perl-moo-2',
                     'perl-multidimensional',
                     'perl-number-range',
                     'perl-path-iterator-rule',
                     'perl-pod-constants',
                     'perl-regexp-pattern',
                     'perl-regexp-pattern-license',
                     'perl-software-license',
                     'perl-string-copyright',
                     'perl-string-escape',
                     'perl-sub-quote',
                     'perl-test-failwarnings',
                     'perl-test-filename',
                     'perl-test-roo',
                     'perl-universal-require',
                     'perl-uri-escape',
                     'php-with-bcmath',
                     'pscircle',
                     'python-backports-csv',
                     'python-betamax-matchers',
                     'python-clf',
                     'python-fasteners',
                     'python-ghp-import',
                     'python-internetarchive',
                     'python-iso3166',
                     'python-iso639',
                     'python-jsonpatch',
                     'python-jsonpatch',
                     'python-pyaudio',
                     'python-pycryptodome',
                     'python-schema',
                     'python-schema',
                     'python-send2trash',
                     'python-uritemplate',
                     'python-xmltodict',
                     'r-colorout',
                     'restic',
                     'sipcalc',
                     'streamlink',
                     'tmux-xpanes',
                     'tome4',
                     'twitchy',
                     'ubridge',
                     'xmobar'].each{
                        guix.build it
                    }
                }
            }
        }
    }
    post {
        always {
            sendNotifications currentBuild.result
        }
    }
}





