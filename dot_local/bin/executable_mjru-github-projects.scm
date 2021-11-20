#!/run/current-system/profile/bin/guile \
--no-auto-compile -e (mjru-github-projects) -s
!#

(define-module (mjru-github-projects)
  #:use-module (guix build utils)
  #:use-module (guix discovery)
  #:use-module (guix records)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (guile pass)
  #:export (main))

(add-to-load-path "/home/oleg/.local/share/chezmoi/dotfiles")

(define-record-type* <git-project>
  git-project make-git-project
  git-project?
  (name git-project-name)
  (group git-project-group)
  (output git-project-output (default #f)))

(define* (fold-projects proc init
                     #:optional
                     (modules (list (resolve-module '(projects)))))
  "For each git-project type exported by one of MODULES, call (PROC RESULT).
INIT is used as the initial value of RESULT."
  (fold-module-public-variables (lambda (object result)
                                  (if (git-project? object)
                                      (proc object result)
                                      result))
                                init
                                modules))

(define-public git-project-ci-bfg
  (git-project
    (name "bfg")
    (group "ci")
    (output
      "/home/oleg/majordomo/_ci/bfg")))

(define-public git-project-ci-cfssl
  (git-project
    (name "cfssl")
    (group "ci")
    (output
      "/home/oleg/majordomo/_ci/cfssl")))

(define-public git-project-ci-chatops
  (git-project
    (name "chatops")
    (group "ci")
    (output
      "/home/oleg/majordomo/_ci/chatops")))

(define-public git-project-ci-chef-workstation
  (git-project
    (name "chef-workstation")
    (group "ci")
    (output
      "/home/oleg/majordomo/_ci/chef-workstation")))

(define-public git-project-ci-dh-docker-swarm-vm
  (git-project
    (name "dh-docker-swarm-vm")
    (group "ci")
    (output
      "/home/oleg/majordomo/_ci/dh-docker-swarm-vm")))

(define-public git-project-ci-docker-stacks
  (git-project
    (name "docker-stacks")
    (group "ci")
    (output
      "/home/oleg/majordomo/_ci/docker-stacks")))

(define-public git-project-ci-hms
  (git-project
    (name "hms")
    (group "ci")
    (output
      "/home/oleg/majordomo/_ci/hms")))

(define-public git-project-ci-jenkins
  (git-project
    (name "jenkins")
    (group "ci")
    (output
      "/home/oleg/majordomo/_ci/jenkins")))

(define-public git-project-ci-jenkins-nix
  (git-project
    (name "jenkins-nix")
    (group "ci")
    (output
      "/home/oleg/majordomo/_ci/jenkins-nix")))

(define-public git-project-ci-jenkins-nix-plugins
  (git-project
    (name "jenkins-nix-plugins")
    (group "ci")
    (output
      "/home/oleg/majordomo/_ci/jenkins-nix-plugins")))

(define-public git-project-ci-jenkins-scriptler
  (git-project
    (name "jenkins-scriptler")
    (group "ci")
    (output
      "/home/oleg/majordomo/_ci/jenkins-scriptler")))

(define-public git-project-ci-joker-compose
  (git-project
    (name "joker-compose")
    (group "ci")
    (output
      "/home/oleg/majordomo/_ci/joker-compose")))

(define-public git-project-ci-maintenance
  (git-project
    (name "maintenance")
    (group "ci")
    (output
      "/home/oleg/majordomo/_ci/maintenance")))

(define-public git-project-ci-maintenance-github
  (git-project
    (name "maintenance-github")
    (group "ci")
    (output
      "/home/oleg/majordomo/_ci/maintenance-github")))

(define-public git-project-ci-maintenance-gitlab-com
  (git-project
    (name "maintenance-gitlab-com")
    (group "ci")
    (output
      "/home/oleg/majordomo/_ci/maintenance-gitlab-com")))

(define-public git-project-ci-maintenance-gitlab-intr
  (git-project
    (name "maintenance-gitlab-intr")
    (group "ci")
    (output
      "/home/oleg/majordomo/_ci/maintenance-gitlab-intr")))

(define-public git-project-ci-nexus
  (git-project
    (name "nexus")
    (group "ci")
    (output
      "/home/oleg/majordomo/_ci/nexus")))

(define-public git-project-ci-nginx
  (git-project
    (name "nginx")
    (group "ci")
    (output
      "/home/oleg/majordomo/_ci/nginx")))

(define-public git-project-ci-nix-flake-update
  (git-project
    (name "nix-flake-update")
    (group "ci")
    (output
      "/home/oleg/majordomo/_ci/nix-flake-update")))

(define-public git-project-ci-nix-store-gc
  (git-project
    (name "nix-store-gc")
    (group "ci")
    (output
      "/home/oleg/majordomo/_ci/nix-store-gc")))

(define-public git-project-ci-nixops
  (git-project
    (name "nixops")
    (group "ci")
    (output
      "/home/oleg/majordomo/_ci/nixops")))

(define-public git-project-ci-nixos-docker
  (git-project
    (name "nixos-docker")
    (group "ci")
    (output
      "/home/oleg/majordomo/_ci/nixos-docker")))

(define-public git-project-ci-nixpkgs
  (git-project
    (name "nixpkgs")
    (group "ci")
    (output
      "/home/oleg/majordomo/_ci/nixpkgs")))

(define-public git-project-ci-packer
  (git-project
    (name "packer")
    (group "ci")
    (output
      "/home/oleg/majordomo/_ci/packer")))

(define-public git-project-ci-pipeline-shared-libs
  (git-project
    (name "pipeline-shared-libs")
    (group "ci")
    (output
      "/home/oleg/majordomo/_ci/pipeline-shared-libs")))

(define-public git-project-ci-vault
  (git-project
    (name "vault")
    (group "ci")
    (output
      "/home/oleg/majordomo/_ci/vault")))

(define-public git-project-ansible-alerta-deploy
  (git-project
    (name "alerta-deploy")
    (group "ansible")
    (output
      "/home/oleg/majordomo/ansible/alerta-deploy")))

(define-public git-project-ansible-docker-provision
  (git-project
    (name "docker-provision")
    (group "ansible")
    (output
      "/home/oleg/majordomo/ansible/docker-provision")))

(define-public git-project-ansible-dockerhost
  (git-project
    (name "dockerhost")
    (group "ansible")
    (output
      "/home/oleg/majordomo/ansible/dockerhost")))

(define-public git-project-ansible-filebeat
  (git-project
    (name "filebeat")
    (group "ansible")
    (output
      "/home/oleg/majordomo/ansible/filebeat")))

(define-public git-project-ansible-kvm
  (git-project
    (name "kvm")
    (group "ansible")
    (output
      "/home/oleg/majordomo/ansible/kvm")))

(define-public git-project-ansible-mj-ns
  (git-project
    (name "mj-ns")
    (group "ansible")
    (output
      "/home/oleg/majordomo/ansible/mj-ns")))

(define-public git-project-ansible-shared
  (git-project
    (name "shared")
    (group "ansible")
    (output
      "/home/oleg/majordomo/ansible/shared")))

(define-public git-project-ansible-telegraf
  (git-project
    (name "telegraf")
    (group "ansible")
    (output
      "/home/oleg/majordomo/ansible/telegraf")))

(define-public git-project-apps-bitrix
  (git-project
    (name "bitrix")
    (group "apps")
    (output
      "/home/oleg/majordomo/apps/bitrix")))

(define-public git-project-apps-bitrix-start
  (git-project
    (name "bitrix-start")
    (group "apps")
    (output
      "/home/oleg/majordomo/apps/bitrix-start")))

(define-public git-project-apps-joomla
  (git-project
    (name "joomla")
    (group "apps")
    (output
      "/home/oleg/majordomo/apps/joomla")))

(define-public git-project-apps-moodle
  (git-project
    (name "moodle")
    (group "apps")
    (output
      "/home/oleg/majordomo/apps/moodle")))

(define-public git-project-apps-moodle-language
  (git-project
    (name "moodle-language")
    (group "apps")
    (output
      "/home/oleg/majordomo/apps/moodle-language")))

(define-public git-project-apps-opencart
  (git-project
    (name "opencart")
    (group "apps")
    (output
      "/home/oleg/majordomo/apps/opencart")))

(define-public git-project-apps-opencart-rus
  (git-project
    (name "opencart-rus")
    (group "apps")
    (output
      "/home/oleg/majordomo/apps/opencart-rus")))

(define-public git-project-apps-wordpress
  (git-project
    (name "wordpress")
    (group "apps")
    (output
      "/home/oleg/majordomo/apps/wordpress")))

(define-public git-project-arenda-servera-centos-7
  (git-project
    (name "centos-7")
    (group "arenda-servera")
    (output
      "/home/oleg/majordomo/arenda-servera/centos-7")))

(define-public git-project-backup-ftpserver
  (git-project
    (name "ftpserver")
    (group "backup")
    (output
      "/home/oleg/majordomo/backup/ftpserver")))

(define-public git-project-backup-rclone
  (git-project
    (name "rclone")
    (group "backup")
    (output
      "/home/oleg/majordomo/backup/rclone")))

(define-public git-project-backup-restic-prometheus-exporter
  (git-project
    (name "restic-prometheus-exporter")
    (group "backup")
    (output
      "/home/oleg/majordomo/backup/restic-prometheus-exporter")))

(define-public git-project-base-apachebox
  (git-project
    (name "apachebox")
    (group "base")
    (output
      "/home/oleg/majordomo/base/apachebox")))

(define-public git-project-base-javabox
  (git-project
    (name "javabox")
    (group "base")
    (output
      "/home/oleg/majordomo/base/javabox")))

(define-public git-project-base-mj-composer
  (git-project
    (name "mj-composer")
    (group "base")
    (output
      "/home/oleg/majordomo/base/mj-composer")))

(define-public git-project-base-nexus
  (git-project
    (name "nexus")
    (group "base")
    (output
      "/home/oleg/majordomo/base/nexus")))

(define-public git-project-base-restic
  (git-project
    (name "restic")
    (group "base")
    (output
      "/home/oleg/majordomo/base/restic")))

(define-public git-project-base-shared-build-php
  (git-project
    (name "shared-build-php")
    (group "base")
    (output
      "/home/oleg/majordomo/base/shared-build-php")))

(define-public git-project-billing2-billing
  (git-project
    (name "billing")
    (group "billing2")
    (output
      "/home/oleg/majordomo/billing2/billing")))

(define-public git-project-billing2-billing2-cron
  (git-project
    (name "billing2-cron")
    (group "billing2")
    (output
      "/home/oleg/majordomo/billing2/billing2-cron")))

(define-public git-project-billing2-control
  (git-project
    (name "control")
    (group "billing2")
    (output
      "/home/oleg/majordomo/billing2/control")))

(define-public git-project-billing2-mj-rpc
  (git-project
    (name "mj-rpc")
    (group "billing2")
    (output
      "/home/oleg/majordomo/billing2/mj-rpc")))

(define-public git-project-billing2-payment-listener
  (git-project
    (name "payment-listener")
    (group "billing2")
    (output
      "/home/oleg/majordomo/billing2/payment-listener")))

(define-public git-project-billing2-phpverchanger
  (git-project
    (name "phpverchanger")
    (group "billing2")
    (output
      "/home/oleg/majordomo/billing2/phpverchanger")))

(define-public git-project-chef-bareos_clients
  (git-project
    (name "bareos_clients")
    (group "chef")
    (output
      "/home/oleg/majordomo/chef/bareos_clients")))

(define-public git-project-chef-bareos_server
  (git-project
    (name "bareos_server")
    (group "chef")
    (output
      "/home/oleg/majordomo/chef/bareos_server")))

(define-public git-project-chef-chef-callbacks
  (git-project
    (name "chef-callbacks")
    (group "chef")
    (output
      "/home/oleg/majordomo/chef/chef-callbacks")))

(define-public git-project-chef-colo
  (git-project
    (name "colo")
    (group "chef")
    (output
      "/home/oleg/majordomo/chef/colo")))

(define-public git-project-chef-cron
  (git-project
    (name "cron")
    (group "chef")
    (output
      "/home/oleg/majordomo/chef/cron")))

(define-public git-project-chef-git
  (git-project
    (name "git")
    (group "chef")
    (output
      "/home/oleg/majordomo/chef/git")))

(define-public git-project-chef-kvm
  (git-project
    (name "kvm")
    (group "chef")
    (output
      "/home/oleg/majordomo/chef/kvm")))

(define-public git-project-chef-kvm-dev
  (git-project
    (name "kvm-dev")
    (group "chef")
    (output
      "/home/oleg/majordomo/chef/kvm-dev")))

(define-public git-project-chef-kvm_admin
  (git-project
    (name "kvm_admin")
    (group "chef")
    (output
      "/home/oleg/majordomo/chef/kvm_admin")))

(define-public git-project-chef-registrantru
  (git-project
    (name "registrantru")
    (group "chef")
    (output
      "/home/oleg/majordomo/chef/registrantru")))

(define-public git-project-chef-rsyslog
  (git-project
    (name "rsyslog")
    (group "chef")
    (output
      "/home/oleg/majordomo/chef/rsyslog")))

(define-public git-project-chef-service
  (git-project
    (name "service")
    (group "chef")
    (output
      "/home/oleg/majordomo/chef/service")))

(define-public git-project-chef-sup_jail
  (git-project
    (name "sup_jail")
    (group "chef")
    (output
      "/home/oleg/majordomo/chef/sup_jail")))

(define-public git-project-db-maxscale
  (git-project
    (name "maxscale")
    (group "db")
    (output
      "/home/oleg/majordomo/db/maxscale")))

(define-public git-project-db-mongo
  (git-project
    (name "mongo")
    (group "db")
    (output
      "/home/oleg/majordomo/db/mongo")))

(define-public git-project-dcs-billing
  (git-project
    (name "billing")
    (group "dcs")
    (output
      "/home/oleg/majordomo/dcs/billing")))

(define-public git-project-dcs-config-repo
  (git-project
    (name "config-repo")
    (group "dcs")
    (output
      "/home/oleg/majordomo/dcs/config-repo")))

(define-public git-project-dcs-configserver
  (git-project
    (name "configserver")
    (group "dcs")
    (output
      "/home/oleg/majordomo/dcs/configserver")))

(define-public git-project-dcs-eureka
  (git-project
    (name "eureka")
    (group "dcs")
    (output
      "/home/oleg/majordomo/dcs/eureka")))

(define-public git-project-deb_pkgs-postfix
  (git-project
    (name "postfix")
    (group "deb_pkgs")
    (output
      "/home/oleg/majordomo/deb_pkgs/postfix")))

(define-public git-project-doc-lsi
  (git-project
    (name "lsi")
    (group "doc")
    (output
      "/home/oleg/majordomo/doc/lsi")))

(define-public git-project-doc-papersplease
  (git-project
    (name "papersplease")
    (group "doc")
    (output
      "/home/oleg/majordomo/doc/papersplease")))

(define-public git-project-doc-support-test
  (git-project
    (name "support-test")
    (group "doc")
    (output
      "/home/oleg/majordomo/doc/support-test")))

(define-public git-project-dockerfiles-TICK
  (git-project
    (name "TICK")
    (group "dockerfiles")
    (output
      "/home/oleg/majordomo/dockerfiles/TICK")))

(define-public git-project-dockerfiles-bareos
  (git-project
    (name "bareos")
    (group "dockerfiles")
    (output
      "/home/oleg/majordomo/dockerfiles/bareos")))

(define-public git-project-dockerfiles-elk
  (git-project
    (name "elk")
    (group "dockerfiles")
    (output
      "/home/oleg/majordomo/dockerfiles/elk")))

(define-public git-project-dockerfiles-fedora25-exim-builder
  (git-project
    (name "fedora25-exim-builder")
    (group "dockerfiles")
    (output
      "/home/oleg/majordomo/dockerfiles/fedora25-exim-builder")))

(define-public git-project-dockerfiles-galeratest
  (git-project
    (name "galeratest")
    (group "dockerfiles")
    (output
      "/home/oleg/majordomo/dockerfiles/galeratest")))

(define-public git-project-dockerfiles-graphite-grafana
  (git-project
    (name "graphite-grafana")
    (group "dockerfiles")
    (output
      "/home/oleg/majordomo/dockerfiles/graphite-grafana")))

(define-public git-project-dockerfiles-http-fileserver
  (git-project
    (name "http-fileserver")
    (group "dockerfiles")
    (output
      "/home/oleg/majordomo/dockerfiles/http-fileserver")))

(define-public git-project-dockerfiles-java
  (git-project
    (name "java")
    (group "dockerfiles")
    (output
      "/home/oleg/majordomo/dockerfiles/java")))

(define-public git-project-dockerfiles-jenkins
  (git-project
    (name "jenkins")
    (group "dockerfiles")
    (output
      "/home/oleg/majordomo/dockerfiles/jenkins")))

(define-public git-project-dockerfiles-mailsystem
  (git-project
    (name "mailsystem")
    (group "dockerfiles")
    (output
      "/home/oleg/majordomo/dockerfiles/mailsystem")))

(define-public git-project-dockerfiles-mj-clamav
  (git-project
    (name "mj-clamav")
    (group "dockerfiles")
    (output
      "/home/oleg/majordomo/dockerfiles/mj-clamav")))

(define-public git-project-dockerfiles-mj-mail
  (git-project
    (name "mj-mail")
    (group "dockerfiles")
    (output
      "/home/oleg/majordomo/dockerfiles/mj-mail")))

(define-public git-project-dockerfiles-mj-openldap
  (git-project
    (name "mj-openldap")
    (group "dockerfiles")
    (output
      "/home/oleg/majordomo/dockerfiles/mj-openldap")))

(define-public git-project-dockerfiles-pdns_mysql
  (git-project
    (name "pdns_mysql")
    (group "dockerfiles")
    (output
      "/home/oleg/majordomo/dockerfiles/pdns_mysql")))

(define-public git-project-dockerfiles-pipelines
  (git-project
    (name "pipelines")
    (group "dockerfiles")
    (output
      "/home/oleg/majordomo/dockerfiles/pipelines")))

(define-public git-project-dockerfiles-rabbitmq
  (git-project
    (name "rabbitmq")
    (group "dockerfiles")
    (output
      "/home/oleg/majordomo/dockerfiles/rabbitmq")))

(define-public git-project-dockerfiles-rabbitmq-swarm-cluster
  (git-project
    (name "rabbitmq-swarm-cluster")
    (group "dockerfiles")
    (output
      "/home/oleg/majordomo/dockerfiles/rabbitmq-swarm-cluster")))

(define-public git-project-dockerfiles-repo
  (git-project
    (name "repo")
    (group "dockerfiles")
    (output
      "/home/oleg/majordomo/dockerfiles/repo")))

(define-public git-project-dockerfiles-s6-overlay
  (git-project
    (name "s6-overlay")
    (group "dockerfiles")
    (output
      "/home/oleg/majordomo/dockerfiles/s6-overlay")))

(define-public git-project-dockerfiles-ubuntu1804-exim-builder
  (git-project
    (name "ubuntu1804-exim-builder")
    (group "dockerfiles")
    (output
      "/home/oleg/majordomo/dockerfiles/ubuntu1804-exim-builder")))

(define-public git-project-dockerfiles-webftp
  (git-project
    (name "webftp")
    (group "dockerfiles")
    (output
      "/home/oleg/majordomo/dockerfiles/webftp")))

(define-public git-project-dockerfiles-wkhtml2pdf
  (git-project
    (name "wkhtml2pdf")
    (group "dockerfiles")
    (output
      "/home/oleg/majordomo/dockerfiles/wkhtml2pdf")))

(define-public git-project-dockerfiles-xhtml2pdf
  (git-project
    (name "xhtml2pdf")
    (group "dockerfiles")
    (output
      "/home/oleg/majordomo/dockerfiles/xhtml2pdf")))

(define-public git-project-domains-control
  (git-project
    (name "control")
    (group "domains")
    (output
      "/home/oleg/majordomo/domains/control")))

(define-public git-project-domains-dit
  (git-project
    (name "dit")
    (group "domains")
    (output
      "/home/oleg/majordomo/domains/dit")))

(define-public git-project-domains-expired-page
  (git-project
    (name "expired-page")
    (group "domains")
    (output
      "/home/oleg/majordomo/domains/expired-page")))

(define-public git-project-domains-nethouse-redirect
  (git-project
    (name "nethouse-redirect")
    (group "domains")
    (output
      "/home/oleg/majordomo/domains/nethouse-redirect")))

(define-public git-project-domains-oauth-registrant
  (git-project
    (name "oauth-registrant")
    (group "domains")
    (output
      "/home/oleg/majordomo/domains/oauth-registrant")))

(define-public git-project-domains-payment-listener-domains
  (git-project
    (name "payment-listener-domains")
    (group "domains")
    (output
      "/home/oleg/majordomo/domains/payment-listener-domains")))

(define-public git-project-domains-redirector
  (git-project
    (name "redirector")
    (group "domains")
    (output
      "/home/oleg/majordomo/domains/redirector")))

(define-public git-project-domains-reg-admin
  (git-project
    (name "reg-admin")
    (group "domains")
    (output
      "/home/oleg/majordomo/domains/reg-admin")))

(define-public git-project-domains-reg-rpc
  (git-project
    (name "reg-rpc")
    (group "domains")
    (output
      "/home/oleg/majordomo/domains/reg-rpc")))

(define-public git-project-domains-reg-rpc-cron
  (git-project
    (name "reg-rpc-cron")
    (group "domains")
    (output
      "/home/oleg/majordomo/domains/reg-rpc-cron")))

(define-public git-project-domains-staff-registrant
  (git-project
    (name "staff-registrant")
    (group "domains")
    (output
      "/home/oleg/majordomo/domains/staff-registrant")))

(define-public git-project-domains-whois
  (git-project
    (name "whois")
    (group "domains")
    (output
      "/home/oleg/majordomo/domains/whois")))

(define-public git-project-go-hms-libs
  (git-project
    (name "libs")
    (group "go-hms")
    (output
      "/home/oleg/majordomo/go-hms/libs")))

(define-public git-project-hms-alerta-client-starter
  (git-project
    (name "alerta-client-starter")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/alerta-client-starter")))

(define-public git-project-hms-alien-domains-searcher
  (git-project
    (name "alien-domains-searcher")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/alien-domains-searcher")))

(define-public git-project-hms-apigw
  (git-project
    (name "apigw")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/apigw")))

(define-public git-project-hms-appscat
  (git-project
    (name "appscat")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/appscat")))

(define-public git-project-hms-archifuck
  (git-project
    (name "archifuck")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/archifuck")))

(define-public git-project-hms-backup
  (git-project
    (name "backup")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/backup")))

(define-public git-project-hms-bizmail
  (git-project
    (name "bizmail")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/bizmail")))

(define-public git-project-hms-blacklist
  (git-project
    (name "blacklist")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/blacklist")))

(define-public git-project-hms-cli-utils
  (git-project
    (name "cli-utils")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/cli-utils")))

(define-public git-project-hms-common
  (git-project
    (name "common")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/common")))

(define-public git-project-hms-config-repo
  (git-project
    (name "config-repo")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/config-repo")))

(define-public git-project-hms-config-server
  (git-project
    (name "config-server")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/config-server")))

(define-public git-project-hms-config-template-repo
  (git-project
    (name "config-template-repo")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/config-template-repo")))

(define-public git-project-hms-cronometr
  (git-project
    (name "cronometr")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/cronometr")))

(define-public git-project-hms-docker-compose
  (git-project
    (name "docker-compose")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/docker-compose")))

(define-public git-project-hms-domain-registrar
  (git-project
    (name "domain-registrar")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/domain-registrar")))

(define-public git-project-hms-eureka
  (git-project
    (name "eureka")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/eureka")))

(define-public git-project-hms-finansier
  (git-project
    (name "finansier")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/finansier")))

(define-public git-project-hms-frontend-app
  (git-project
    (name "frontend-app")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/frontend-app")))

(define-public git-project-hms-hardy
  (git-project
    (name "hardy")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/hardy")))

(define-public git-project-hms-hardy-frontend
  (git-project
    (name "hardy-frontend")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/hardy-frontend")))

(define-public git-project-hms-hms
  (git-project
    (name "hms")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/hms")))

(define-public git-project-hms-hms-client-spring-boot-starter
  (git-project
    (name "hms-client-spring-boot-starter")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/hms-client-spring-boot-starter")))

(define-public git-project-hms-legacy-scripts
  (git-project
    (name "legacy-scripts")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/legacy-scripts")))

(define-public git-project-hms-letsencrypt
  (git-project
    (name "letsencrypt")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/letsencrypt")))

(define-public git-project-hms-majordomo-ru
  (git-project
    (name "majordomo-ru")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/majordomo-ru")))

(define-public git-project-hms-mj-swagger-ui
  (git-project
    (name "mj-swagger-ui")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/mj-swagger-ui")))

(define-public git-project-hms-mlp
  (git-project
    (name "mlp")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/mlp")))

(define-public git-project-hms-mobile-app
  (git-project
    (name "mobile-app")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/mobile-app")))

(define-public git-project-hms-mocking-rabbit-with-qpid
  (git-project
    (name "mocking-rabbit-with-qpid")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/mocking-rabbit-with-qpid")))

(define-public git-project-hms-monitoring
  (git-project
    (name "monitoring")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/monitoring")))

(define-public git-project-hms-notifier
  (git-project
    (name "notifier")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/notifier")))

(define-public git-project-hms-overseer
  (git-project
    (name "overseer")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/overseer")))

(define-public git-project-hms-partners
  (git-project
    (name "partners")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/partners")))

(define-public git-project-hms-payment-listeners
  (git-project
    (name "payment-listeners")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/payment-listeners")))

(define-public git-project-hms-personmgr
  (git-project
    (name "personmgr")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/personmgr")))

(define-public git-project-hms-powerdns-client
  (git-project
    (name "powerdns-client")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/powerdns-client")))

(define-public git-project-hms-quotaback
  (git-project
    (name "quotaback")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/quotaback")))

(define-public git-project-hms-rc-staff
  (git-project
    (name "rc-staff")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/rc-staff")))

(define-public git-project-hms-rc-user
  (git-project
    (name "rc-user")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/rc-user")))

(define-public git-project-hms-sbis
  (git-project
    (name "sbis")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/sbis")))

(define-public git-project-hms-si
  (git-project
    (name "si")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/si")))

(define-public git-project-hms-sphinx
  (git-project
    (name "sphinx")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/sphinx")))

(define-public git-project-hms-spring-hello-world
  (git-project
    (name "spring-hello-world")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/spring-hello-world")))

(define-public git-project-hms-staff-frontend-app
  (git-project
    (name "staff-frontend-app")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/staff-frontend-app")))

(define-public git-project-hms-stat
  (git-project
    (name "stat")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/stat")))

(define-public git-project-hms-storage
  (git-project
    (name "storage")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/storage")))

(define-public git-project-hms-taskexecutor
  (git-project
    (name "taskexecutor")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/taskexecutor")))

(define-public git-project-hms-telegram
  (git-project
    (name "telegram")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/telegram")))

(define-public git-project-hms-teremock
  (git-project
    (name "teremock")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/teremock")))

(define-public git-project-hms-term
  (git-project
    (name "term")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/term")))

(define-public git-project-hms-tracker
  (git-project
    (name "tracker")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/tracker")))

(define-public git-project-hms-tracker-frontend
  (git-project
    (name "tracker-frontend")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/tracker-frontend")))

(define-public git-project-hms-web
  (git-project
    (name "web")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/web")))

(define-public git-project-hms-yandex-kassa-client-spring-boot-starter
  (git-project
    (name "yandex-kassa-client-spring-boot-starter")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/yandex-kassa-client-spring-boot-starter")))

(define-public git-project-hms-yandex-promoter
  (git-project
    (name "yandex-promoter")
    (group "hms")
    (output
      "/home/oleg/majordomo/hms/yandex-promoter")))

(define-public git-project-jenkins-test-project-test
  (git-project
    (name "project-test")
    (group "jenkins-test")
    (output
      "/home/oleg/majordomo/jenkins-test/project-test")))

(define-public git-project-kvm-templates-bento
  (git-project
    (name "bento")
    (group "kvm-templates")
    (output
      "/home/oleg/majordomo/kvm-templates/bento")))

(define-public git-project-kvm-templates-centos6
  (git-project
    (name "centos6")
    (group "kvm-templates")
    (output
      "/home/oleg/majordomo/kvm-templates/centos6")))

(define-public git-project-kvm-templates-centos7
  (git-project
    (name "centos7")
    (group "kvm-templates")
    (output
      "/home/oleg/majordomo/kvm-templates/centos7")))

(define-public git-project-kvm-templates-debian10
  (git-project
    (name "debian10")
    (group "kvm-templates")
    (output
      "/home/oleg/majordomo/kvm-templates/debian10")))

(define-public git-project-kvm-templates-debian8
  (git-project
    (name "debian8")
    (group "kvm-templates")
    (output
      "/home/oleg/majordomo/kvm-templates/debian8")))

(define-public git-project-kvm-templates-debian8a
  (git-project
    (name "debian8a")
    (group "kvm-templates")
    (output
      "/home/oleg/majordomo/kvm-templates/debian8a")))

(define-public git-project-kvm-templates-debian9
  (git-project
    (name "debian9")
    (group "kvm-templates")
    (output
      "/home/oleg/majordomo/kvm-templates/debian9")))

(define-public git-project-kvm-templates-debian9a
  (git-project
    (name "debian9a")
    (group "kvm-templates")
    (output
      "/home/oleg/majordomo/kvm-templates/debian9a")))

(define-public git-project-kvm-templates-packer
  (git-project
    (name "packer")
    (group "kvm-templates")
    (output
      "/home/oleg/majordomo/kvm-templates/packer")))

(define-public git-project-kvm-templates-packer-centos-6
  (git-project
    (name "packer-centos-6")
    (group "kvm-templates")
    (output
      "/home/oleg/majordomo/kvm-templates/packer-centos-6")))

(define-public git-project-kvm-templates-packer-centos-7
  (git-project
    (name "packer-centos-7")
    (group "kvm-templates")
    (output
      "/home/oleg/majordomo/kvm-templates/packer-centos-7")))

(define-public git-project-kvm-templates-packer-centos-8
  (git-project
    (name "packer-centos-8")
    (group "kvm-templates")
    (output
      "/home/oleg/majordomo/kvm-templates/packer-centos-8")))

(define-public git-project-kvm-templates-packer-debian-10
  (git-project
    (name "packer-debian-10")
    (group "kvm-templates")
    (output
      "/home/oleg/majordomo/kvm-templates/packer-debian-10")))

(define-public git-project-kvm-templates-packer-debian-11
  (git-project
    (name "packer-debian-11")
    (group "kvm-templates")
    (output
      "/home/oleg/majordomo/kvm-templates/packer-debian-11")))

(define-public git-project-kvm-templates-packer-debian-8
  (git-project
    (name "packer-debian-8")
    (group "kvm-templates")
    (output
      "/home/oleg/majordomo/kvm-templates/packer-debian-8")))

(define-public git-project-kvm-templates-packer-debian-9
  (git-project
    (name "packer-debian-9")
    (group "kvm-templates")
    (output
      "/home/oleg/majordomo/kvm-templates/packer-debian-9")))

(define-public git-project-kvm-templates-packer-ubuntu-14-04
  (git-project
    (name "packer-ubuntu-14-04")
    (group "kvm-templates")
    (output
      "/home/oleg/majordomo/kvm-templates/packer-ubuntu-14-04")))

(define-public git-project-kvm-templates-packer-ubuntu-16-04
  (git-project
    (name "packer-ubuntu-16-04")
    (group "kvm-templates")
    (output
      "/home/oleg/majordomo/kvm-templates/packer-ubuntu-16-04")))

(define-public git-project-kvm-templates-packer-ubuntu-18-04
  (git-project
    (name "packer-ubuntu-18-04")
    (group "kvm-templates")
    (output
      "/home/oleg/majordomo/kvm-templates/packer-ubuntu-18-04")))

(define-public git-project-kvm-templates-packer-ubuntu-20-04
  (git-project
    (name "packer-ubuntu-20-04")
    (group "kvm-templates")
    (output
      "/home/oleg/majordomo/kvm-templates/packer-ubuntu-20-04")))

(define-public git-project-kvm-templates-ubuntu-20.04
  (git-project
    (name "ubuntu-20.04")
    (group "kvm-templates")
    (output
      "/home/oleg/majordomo/kvm-templates/ubuntu-20.04")))

(define-public git-project-kvm-templates-ubuntu14lts
  (git-project
    (name "ubuntu14lts")
    (group "kvm-templates")
    (output
      "/home/oleg/majordomo/kvm-templates/ubuntu14lts")))

(define-public git-project-kvm-templates-ubuntu16lts
  (git-project
    (name "ubuntu16lts")
    (group "kvm-templates")
    (output
      "/home/oleg/majordomo/kvm-templates/ubuntu16lts")))

(define-public git-project-kvm-templates-ubuntu18lts
  (git-project
    (name "ubuntu18lts")
    (group "kvm-templates")
    (output
      "/home/oleg/majordomo/kvm-templates/ubuntu18lts")))

(define-public git-project-legacy-chef-server
  (git-project
    (name "chef-server")
    (group "legacy")
    (output
      "/home/oleg/majordomo/legacy/chef-server")))

(define-public git-project-legacy-chef-workstation
  (git-project
    (name "chef-workstation")
    (group "legacy")
    (output
      "/home/oleg/majordomo/legacy/chef-workstation")))

(define-public git-project-mail-freshclam
  (git-project
    (name "freshclam")
    (group "mail")
    (output
      "/home/oleg/majordomo/mail/freshclam")))

(define-public git-project-mail-mail-checker
  (git-project
    (name "mail-checker")
    (group "mail")
    (output
      "/home/oleg/majordomo/mail/mail-checker")))

(define-public git-project-mail-mail-storage-mda
  (git-project
    (name "mail-storage-mda")
    (group "mail")
    (output
      "/home/oleg/majordomo/mail/mail-storage-mda")))

(define-public git-project-mail-mail-storage-mta
  (git-project
    (name "mail-storage-mta")
    (group "mail")
    (output
      "/home/oleg/majordomo/mail/mail-storage-mta")))

(define-public git-project-mail-mail-tests
  (git-project
    (name "mail-tests")
    (group "mail")
    (output
      "/home/oleg/majordomo/mail/mail-tests")))

(define-public git-project-mail-mailing_manager
  (git-project
    (name "mailing_manager")
    (group "mail")
    (output
      "/home/oleg/majordomo/mail/mailing_manager")))

(define-public git-project-mail-mda-proxy
  (git-project
    (name "mda-proxy")
    (group "mail")
    (output
      "/home/oleg/majordomo/mail/mda-proxy")))

(define-public git-project-mail-mx
  (git-project
    (name "mx")
    (group "mail")
    (output
      "/home/oleg/majordomo/mail/mx")))

(define-public git-project-mail-smtp-out
  (git-project
    (name "smtp-out")
    (group "mail")
    (output
      "/home/oleg/majordomo/mail/smtp-out")))

(define-public git-project-mail-spammer
  (git-project
    (name "spammer")
    (group "mail")
    (output
      "/home/oleg/majordomo/mail/spammer")))

(define-public git-project-mail-webui
  (git-project
    (name "webui")
    (group "mail")
    (output
      "/home/oleg/majordomo/mail/webui")))

(define-public git-project-monitoring-alerta
  (git-project
    (name "alerta")
    (group "monitoring")
    (output
      "/home/oleg/majordomo/monitoring/alerta")))

(define-public git-project-monitoring-alertmanager
  (git-project
    (name "alertmanager")
    (group "monitoring")
    (output
      "/home/oleg/majordomo/monitoring/alertmanager")))

(define-public git-project-monitoring-curator
  (git-project
    (name "curator")
    (group "monitoring")
    (output
      "/home/oleg/majordomo/monitoring/curator")))

(define-public git-project-monitoring-elastalert
  (git-project
    (name "elastalert")
    (group "monitoring")
    (output
      "/home/oleg/majordomo/monitoring/elastalert")))

(define-public git-project-monitoring-elasticsearch
  (git-project
    (name "elasticsearch")
    (group "monitoring")
    (output
      "/home/oleg/majordomo/monitoring/elasticsearch")))

(define-public git-project-monitoring-filebeat
  (git-project
    (name "filebeat")
    (group "monitoring")
    (output
      "/home/oleg/majordomo/monitoring/filebeat")))

(define-public git-project-monitoring-ipmiseld
  (git-project
    (name "ipmiseld")
    (group "monitoring")
    (output
      "/home/oleg/majordomo/monitoring/ipmiseld")))

(define-public git-project-monitoring-kapacitor
  (git-project
    (name "kapacitor")
    (group "monitoring")
    (output
      "/home/oleg/majordomo/monitoring/kapacitor")))

(define-public git-project-monitoring-karma
  (git-project
    (name "karma")
    (group "monitoring")
    (output
      "/home/oleg/majordomo/monitoring/karma")))

(define-public git-project-monitoring-kibana
  (git-project
    (name "kibana")
    (group "monitoring")
    (output
      "/home/oleg/majordomo/monitoring/kibana")))

(define-public git-project-monitoring-logstash
  (git-project
    (name "logstash")
    (group "monitoring")
    (output
      "/home/oleg/majordomo/monitoring/logstash")))

(define-public git-project-monitoring-loudml
  (git-project
    (name "loudml")
    (group "monitoring")
    (output
      "/home/oleg/majordomo/monitoring/loudml")))

(define-public git-project-monitoring-mj-api-render-graphite
  (git-project
    (name "mj-api-render-graphite")
    (group "monitoring")
    (output
      "/home/oleg/majordomo/monitoring/mj-api-render-graphite")))

(define-public git-project-monitoring-mysqlwatcher
  (git-project
    (name "mysqlwatcher")
    (group "monitoring")
    (output
      "/home/oleg/majordomo/monitoring/mysqlwatcher")))

(define-public git-project-monitoring-netconfmon
  (git-project
    (name "netconfmon")
    (group "monitoring")
    (output
      "/home/oleg/majordomo/monitoring/netconfmon")))

(define-public git-project-monitoring-operations-monitor
  (git-project
    (name "operations-monitor")
    (group "monitoring")
    (output
      "/home/oleg/majordomo/monitoring/operations-monitor")))

(define-public git-project-monitoring-prometheus
  (git-project
    (name "prometheus")
    (group "monitoring")
    (output
      "/home/oleg/majordomo/monitoring/prometheus")))

(define-public git-project-monitoring-rbl
  (git-project
    (name "rbl")
    (group "monitoring")
    (output
      "/home/oleg/majordomo/monitoring/rbl")))

(define-public git-project-monitoring-td-agent
  (git-project
    (name "td-agent")
    (group "monitoring")
    (output
      "/home/oleg/majordomo/monitoring/td-agent")))

(define-public git-project-monitoring-telegraf
  (git-project
    (name "telegraf")
    (group "monitoring")
    (output
      "/home/oleg/majordomo/monitoring/telegraf")))

(define-public git-project-monitoring-telegraf-build
  (git-project
    (name "telegraf-build")
    (group "monitoring")
    (output
      "/home/oleg/majordomo/monitoring/telegraf-build")))

(define-public git-project-monitoring-web-alerta
  (git-project
    (name "web-alerta")
    (group "monitoring")
    (output
      "/home/oleg/majordomo/monitoring/web-alerta")))

(define-public git-project-net-dns-intr
  (git-project
    (name "dns-intr")
    (group "net")
    (output
      "/home/oleg/majordomo/net/dns-intr")))

(define-public git-project-net-dns-majordomo.ru
  (git-project
    (name "dns-majordomo.ru")
    (group "net")
    (output
      "/home/oleg/majordomo/net/dns-majordomo.ru")))

(define-public git-project-net-dns-registrant-test
  (git-project
    (name "dns-registrant-test")
    (group "net")
    (output
      "/home/oleg/majordomo/net/dns-registrant-test")))

(define-public git-project-net-dns-registrant.ru
  (git-project
    (name "dns-registrant.ru")
    (group "net")
    (output
      "/home/oleg/majordomo/net/dns-registrant.ru")))

(define-public git-project-net-dns-test
  (git-project
    (name "dns-test")
    (group "net")
    (output
      "/home/oleg/majordomo/net/dns-test")))

(define-public git-project-net-mj-nfsen
  (git-project
    (name "mj-nfsen")
    (group "net")
    (output
      "/home/oleg/majordomo/net/mj-nfsen")))

(define-public git-project-net-net-pxe
  (git-project
    (name "net-pxe")
    (group "net")
    (output
      "/home/oleg/majordomo/net/net-pxe")))

(define-public git-project-net-nf-collector
  (git-project
    (name "nf-collector")
    (group "net")
    (output
      "/home/oleg/majordomo/net/nf-collector")))

(define-public git-project-net-nix-pxe
  (git-project
    (name "nix-pxe")
    (group "net")
    (output
      "/home/oleg/majordomo/net/nix-pxe")))

(define-public git-project-net-openvpn
  (git-project
    (name "openvpn")
    (group "net")
    (output
      "/home/oleg/majordomo/net/openvpn")))

(define-public git-project-net-pacifier
  (git-project
    (name "pacifier")
    (group "net")
    (output
      "/home/oleg/majordomo/net/pacifier")))

(define-public git-project-net-pxe
  (git-project
    (name "pxe")
    (group "net")
    (output
      "/home/oleg/majordomo/net/pxe")))

(define-public git-project-net-vpn
  (git-project
    (name "vpn")
    (group "net")
    (output
      "/home/oleg/majordomo/net/vpn")))

;; (define-public git-project-nix-templates
;;   (git-project
;;     (name "templates")
;;     (group "nix")
;;     (output
;;       "/home/oleg/majordomo/nix/templates")))

(define-public git-project-nixos-aacraid
  (git-project
    (name "aacraid")
    (group "nixos")
    (output
      "/home/oleg/majordomo/nixos/aacraid")))

(define-public git-project-nixos-ci
  (git-project
    (name "ci")
    (group "nixos")
    (output
      "/home/oleg/majordomo/nixos/ci")))

(define-public git-project-nixos-dev
  (git-project
    (name "dev")
    (group "nixos")
    (output
      "/home/oleg/majordomo/nixos/dev")))

(define-public git-project-nixos-flake-registry
  (git-project
    (name "flake-registry")
    (group "nixos")
    (output
      "/home/oleg/majordomo/nixos/flake-registry")))

(define-public git-project-nixos-ispconfig
  (git-project
    (name "ispconfig")
    (group "nixos")
    (output
      "/home/oleg/majordomo/nixos/ispconfig")))

(define-public git-project-nixos-ispconfig.backup
  (git-project
    (name "ispconfig.backup")
    (group "nixos")
    (output
      "/home/oleg/majordomo/nixos/ispconfig.backup")))

(define-public git-project-nixos-jenkins
  (git-project
    (name "jenkins")
    (group "nixos")
    (output
      "/home/oleg/majordomo/nixos/jenkins")))

(define-public git-project-nixos-kvm
  (git-project
    (name "kvm")
    (group "nixos")
    (output
      "/home/oleg/majordomo/nixos/kvm")))

(define-public git-project-nixos-monitoring
  (git-project
    (name "monitoring")
    (group "nixos")
    (output
      "/home/oleg/majordomo/nixos/monitoring")))

(define-public git-project-nixos-nginx
  (git-project
    (name "nginx")
    (group "nixos")
    (output
      "/home/oleg/majordomo/nixos/nginx")))

(define-public git-project-nixos-ns
  (git-project
    (name "ns")
    (group "nixos")
    (output
      "/home/oleg/majordomo/nixos/ns")))

(define-public git-project-nixos-pop
  (git-project
    (name "pop")
    (group "nixos")
    (output
      "/home/oleg/majordomo/nixos/pop")))

(define-public git-project-nixos-router-office
  (git-project
    (name "router-office")
    (group "nixos")
    (output
      "/home/oleg/majordomo/nixos/router-office")))

(define-public git-project-nixos-swarm
  (git-project
    (name "swarm")
    (group "nixos")
    (output
      "/home/oleg/majordomo/nixos/swarm")))

(define-public git-project-nixos-vm
  (git-project
    (name "vm")
    (group "nixos")
    (output
      "/home/oleg/majordomo/nixos/vm")))

(define-public git-project-nixos-web
  (git-project
    (name "web")
    (group "nixos")
    (output
      "/home/oleg/majordomo/nixos/web")))

(define-public git-project-nixos-webmail
  (git-project
    (name "webmail")
    (group "nixos")
    (output
      "/home/oleg/majordomo/nixos/webmail")))

(define-public git-project-office-app-feedback
  (git-project
    (name "app-feedback")
    (group "office")
    (output
      "/home/oleg/majordomo/office/app-feedback")))

(define-public git-project-office-cerberus
  (git-project
    (name "cerberus")
    (group "office")
    (output
      "/home/oleg/majordomo/office/cerberus")))

(define-public git-project-office-cerberus-slack
  (git-project
    (name "cerberus-slack")
    (group "office")
    (output
      "/home/oleg/majordomo/office/cerberus-slack")))

(define-public git-project-office-cert-expire
  (git-project
    (name "cert-expire")
    (group "office")
    (output
      "/home/oleg/majordomo/office/cert-expire")))

(define-public git-project-office-docker-glpi
  (git-project
    (name "docker-glpi")
    (group "office")
    (output
      "/home/oleg/majordomo/office/docker-glpi")))

(define-public git-project-office-mailman
  (git-project
    (name "mailman")
    (group "office")
    (output
      "/home/oleg/majordomo/office/mailman")))

(define-public git-project-office-mjCA-installer
  (git-project
    (name "mjCA-installer")
    (group "office")
    (output
      "/home/oleg/majordomo/office/mjCA-installer")))

(define-public git-project-office-office-majordomo-ru
  (git-project
    (name "office-majordomo-ru")
    (group "office")
    (output
      "/home/oleg/majordomo/office/office-majordomo-ru")))

(define-public git-project-office-pma-intr
  (git-project
    (name "pma-intr")
    (group "office")
    (output
      "/home/oleg/majordomo/office/pma-intr")))

(define-public git-project-office-redmine
  (git-project
    (name "redmine")
    (group "office")
    (output
      "/home/oleg/majordomo/office/redmine")))

(define-public git-project-office-snoop
  (git-project
    (name "snoop")
    (group "office")
    (output
      "/home/oleg/majordomo/office/snoop")))

(define-public git-project-office-snoop-client
  (git-project
    (name "snoop-client")
    (group "office")
    (output
      "/home/oleg/majordomo/office/snoop-client")))

(define-public git-project-office-ssl-certificates
  (git-project
    (name "ssl-certificates")
    (group "office")
    (output
      "/home/oleg/majordomo/office/ssl-certificates")))

(define-public git-project-office-wiki-intr
  (git-project
    (name "wiki-intr")
    (group "office")
    (output
      "/home/oleg/majordomo/office/wiki-intr")))

(define-public git-project-pr-hoster24ru
  (git-project
    (name "hoster24ru")
    (group "pr")
    (output
      "/home/oleg/majordomo/pr/hoster24ru")))

(define-public git-project-pyhalov-php52-extra
  (git-project
    (name "php52-extra")
    (group "pyhalov")
    (output
      "/home/oleg/majordomo/pyhalov/php52-extra")))

(define-public git-project-rezvov-nixos
  (git-project
    (name "nixos")
    (group "rezvov")
    (output
      "/home/oleg/majordomo/rezvov/nixos")))

(define-public git-project-security-dot-gnupg
  (git-project
    (name "dot-gnupg")
    (group "security")
    (output
      "/home/oleg/majordomo/security/dot-gnupg")))

(define-public git-project-security-dot-password-store
  (git-project
    (name "dot-password-store")
    (group "security")
    (output
      "/home/oleg/majordomo/security/dot-password-store")))

(define-public git-project-security-password-store
  (git-project
    (name "password-store")
    (group "security")
    (output
      "/home/oleg/majordomo/security/password-store")))

(define-public git-project-security-vault
  (git-project
    (name "vault")
    (group "security")
    (output
      "/home/oleg/majordomo/security/vault")))

(define-public git-project-security-vault-telegraf
  (git-project
    (name "vault-telegraf")
    (group "security")
    (output
      "/home/oleg/majordomo/security/vault-telegraf")))

(define-public git-project-shared-accounts-transfer
  (git-project
    (name "accounts-transfer")
    (group "shared")
    (output
      "/home/oleg/majordomo/shared/accounts-transfer")))

(define-public git-project-shared-backup-as-archive
  (git-project
    (name "backup-as-archive")
    (group "shared")
    (output
      "/home/oleg/majordomo/shared/backup-as-archive")))

(define-public git-project-shared-build_apache_configs
  (git-project
    (name "build_apache_configs")
    (group "shared")
    (output
      "/home/oleg/majordomo/shared/build_apache_configs")))

(define-public git-project-shared-build_nginx_configs
  (git-project
    (name "build_nginx_configs")
    (group "shared")
    (output
      "/home/oleg/majordomo/shared/build_nginx_configs")))

(define-public git-project-shared-cluster-bil
  (git-project
    (name "cluster-bil")
    (group "shared")
    (output
      "/home/oleg/majordomo/shared/cluster-bil")))

(define-public git-project-shared-cluster_restore
  (git-project
    (name "cluster_restore")
    (group "shared")
    (output
      "/home/oleg/majordomo/shared/cluster_restore")))

(define-public git-project-shared-freebsd-shared-bil
  (git-project
    (name "freebsd-shared-bil")
    (group "shared")
    (output
      "/home/oleg/majordomo/shared/freebsd-shared-bil")))

(define-public git-project-shared-http_errors
  (git-project
    (name "http_errors")
    (group "shared")
    (output
      "/home/oleg/majordomo/shared/http_errors")))

(define-public git-project-shared-linux
  (git-project
    (name "linux")
    (group "shared")
    (output
      "/home/oleg/majordomo/shared/linux")))

(define-public git-project-shared-linux-hms
  (git-project
    (name "linux-hms")
    (group "shared")
    (output
      "/home/oleg/majordomo/shared/linux-hms")))

(define-public git-project-shared-mail
  (git-project
    (name "mail")
    (group "shared")
    (output
      "/home/oleg/majordomo/shared/mail")))

(define-public git-project-shared-mail_lib
  (git-project
    (name "mail_lib")
    (group "shared")
    (output
      "/home/oleg/majordomo/shared/mail_lib")))

(define-public git-project-shared-postfix
  (git-project
    (name "postfix")
    (group "shared")
    (output
      "/home/oleg/majordomo/shared/postfix")))

(define-public git-project-shared-postfix_dbs_ctrl
  (git-project
    (name "postfix_dbs_ctrl")
    (group "shared")
    (output
      "/home/oleg/majordomo/shared/postfix_dbs_ctrl")))

(define-public git-project-shared-pysieved
  (git-project
    (name "pysieved")
    (group "shared")
    (output
      "/home/oleg/majordomo/shared/pysieved")))

(define-public git-project-shared-service_mariadb
  (git-project
    (name "service_mariadb")
    (group "shared")
    (output
      "/home/oleg/majordomo/shared/service_mariadb")))

(define-public git-project-shared-static
  (git-project
    (name "static")
    (group "shared")
    (output
      "/home/oleg/majordomo/shared/static")))

(define-public git-project-staff-ad-majordomo-ru
  (git-project
    (name "ad-majordomo-ru")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/ad-majordomo-ru")))

(define-public git-project-staff-app-feedback
  (git-project
    (name "app-feedback")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/app-feedback")))

(define-public git-project-staff-auction_api
  (git-project
    (name "auction_api")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/auction_api")))

(define-public git-project-staff-auto-send-ispsystem-bill-in-pdf-to-slack
  (git-project
    (name "auto-send-ispsystem-bill-in-pdf-to-slack")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/auto-send-ispsystem-bill-in-pdf-to-slack")))

(define-public git-project-staff-bad-ugly-reg-rpc-api
  (git-project
    (name "bad-ugly-reg-rpc-api")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/bad-ugly-reg-rpc-api")))

(define-public git-project-staff-bareos
  (git-project
    (name "bareos")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/bareos")))

(define-public git-project-staff-billing
  (git-project
    (name "billing")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/billing")))

(define-public git-project-staff-billing1
  (git-project
    (name "billing1")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/billing1")))

(define-public git-project-staff-billing_bin
  (git-project
    (name "billing_bin")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/billing_bin")))

(define-public git-project-staff-biz-mail
  (git-project
    (name "biz-mail")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/biz-mail")))

(define-public git-project-staff-biz-mail-bundle
  (git-project
    (name "biz-mail-bundle")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/biz-mail-bundle")))

(define-public git-project-staff-biz-mail-package
  (git-project
    (name "biz-mail-package")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/biz-mail-package")))

(define-public git-project-staff-cerberus
  (git-project
    (name "cerberus")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/cerberus")))

(define-public git-project-staff-chat
  (git-project
    (name "chat")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/chat")))

(define-public git-project-staff-chef-scripts
  (git-project
    (name "chef-scripts")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/chef-scripts")))

(define-public git-project-staff-control
  (git-project
    (name "control")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/control")))

(define-public git-project-staff-control-majordomo-ru
  (git-project
    (name "control-majordomo-ru")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/control-majordomo-ru")))

(define-public git-project-staff-control-registrant-ru
  (git-project
    (name "control-registrant-ru")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/control-registrant-ru")))

(define-public git-project-staff-control1
  (git-project
    (name "control1")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/control1")))

(define-public git-project-staff-cron_billing
  (git-project
    (name "cron_billing")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/cron_billing")))

(define-public git-project-staff-cron_registrant
  (git-project
    (name "cron_registrant")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/cron_registrant")))

(define-public git-project-staff-cron_servers
  (git-project
    (name "cron_servers")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/cron_servers")))

(define-public git-project-staff-deb-src
  (git-project
    (name "deb-src")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/deb-src")))

(define-public git-project-staff-dev_bot
  (git-project
    (name "dev_bot")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/dev_bot")))

(define-public git-project-staff-domainschecker
  (git-project
    (name "domainschecker")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/domainschecker")))

(define-public git-project-staff-hms
  (git-project
    (name "hms")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/hms")))

(define-public git-project-staff-hms-php-client
  (git-project
    (name "hms-php-client")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/hms-php-client")))

(define-public git-project-staff-lets-encrypt-processor
  (git-project
    (name "lets-encrypt-processor")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/lets-encrypt-processor")))

(define-public git-project-staff-mailing_manager
  (git-project
    (name "mailing_manager")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/mailing_manager")))

(define-public git-project-staff-malwarereport
  (git-project
    (name "malwarereport")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/malwarereport")))

(define-public git-project-staff-mj_registration
  (git-project
    (name "mj_registration")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/mj_registration")))

(define-public git-project-staff-mjproxy
  (git-project
    (name "mjproxy")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/mjproxy")))

(define-public git-project-staff-mjpydbs
  (git-project
    (name "mjpydbs")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/mjpydbs")))

(define-public git-project-staff-nagios-configs
  (git-project
    (name "nagios-configs")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/nagios-configs")))

(define-public git-project-staff-new-epp-client
  (git-project
    (name "new-epp-client")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/new-epp-client")))

(define-public git-project-staff-new-majordomo-ru
  (git-project
    (name "new-majordomo-ru")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/new-majordomo-ru")))

(define-public git-project-staff-nfsen
  (git-project
    (name "nfsen")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/nfsen")))

(define-public git-project-staff-oauth-registrant
  (git-project
    (name "oauth-registrant")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/oauth-registrant")))

(define-public git-project-staff-oauth2
  (git-project
    (name "oauth2")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/oauth2")))

(define-public git-project-staff-ocf-resource-agents
  (git-project
    (name "ocf-resource-agents")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/ocf-resource-agents")))

(define-public git-project-staff-parking
  (git-project
    (name "parking")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/parking")))

(define-public git-project-staff-payment-listeners
  (git-project
    (name "payment-listeners")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/payment-listeners")))

(define-public git-project-staff-port_poller
  (git-project
    (name "port_poller")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/port_poller")))

(define-public git-project-staff-reg-admin
  (git-project
    (name "reg-admin")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/reg-admin")))

(define-public git-project-staff-reg-rpc
  (git-project
    (name "reg-rpc")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/reg-rpc")))

(define-public git-project-staff-reg-rpc-crontab
  (git-project
    (name "reg-rpc-crontab")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/reg-rpc-crontab")))

(define-public git-project-staff-registrant
  (git-project
    (name "registrant")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/registrant")))

(define-public git-project-staff-registrant-webmoney
  (git-project
    (name "registrant-webmoney")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/registrant-webmoney")))

(define-public git-project-staff-rpc
  (git-project
    (name "rpc")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/rpc")))

(define-public git-project-staff-sfphphamlplugin
  (git-project
    (name "sfphphamlplugin")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/sfphphamlplugin")))

(define-public git-project-staff-simpletests
  (git-project
    (name "simpletests")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/simpletests")))

(define-public git-project-staff-stats
  (git-project
    (name "stats")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/stats")))

(define-public git-project-staff-vps_admin_php_version_changer
  (git-project
    (name "vps_admin_php_version_changer")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/vps_admin_php_version_changer")))

(define-public git-project-staff-webftp
  (git-project
    (name "webftp")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/webftp")))

(define-public git-project-staff-whois
  (git-project
    (name "whois")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/whois")))

(define-public git-project-staff-xmlrpc
  (git-project
    (name "xmlrpc")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/xmlrpc")))

(define-public git-project-staff-zabbix-scripts
  (git-project
    (name "zabbix-scripts")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/zabbix-scripts")))

(define-public git-project-staff-zabbix-senders
  (git-project
    (name "zabbix-senders")
    (group "staff")
    (output
      "/home/oleg/majordomo/staff/zabbix-senders")))

(define-public git-project-sup-dotfiles
  (git-project
    (name "dotfiles")
    (group "sup")
    (output
      "/home/oleg/majordomo/sup/dotfiles")))

(define-public git-project-sup-scripts
  (git-project
    (name "scripts")
    (group "sup")
    (output
      "/home/oleg/majordomo/sup/scripts")))

(define-public git-project-utils-below
  (git-project
    (name "below")
    (group "utils")
    (output
      "/home/oleg/majordomo/utils/below")))

(define-public git-project-utils-catj
  (git-project
    (name "catj")
    (group "utils")
    (output
      "/home/oleg/majordomo/utils/catj")))

(define-public git-project-utils-firefox-esr
  (git-project
    (name "firefox-esr")
    (group "utils")
    (output
      "/home/oleg/majordomo/utils/firefox-esr")))

(define-public git-project-utils-homer
  (git-project
    (name "homer")
    (group "utils")
    (output
      "/home/oleg/majordomo/utils/homer")))

(define-public git-project-utils-ipmi
  (git-project
    (name "ipmi")
    (group "utils")
    (output
      "/home/oleg/majordomo/utils/ipmi")))

(define-public git-project-utils-ipmiview
  (git-project
    (name "ipmiview")
    (group "utils")
    (output
      "/home/oleg/majordomo/utils/ipmiview")))

(define-public git-project-utils-openvpn
  (git-project
    (name "openvpn")
    (group "utils")
    (output
      "/home/oleg/majordomo/utils/openvpn")))

(define-public git-project-vds-admin-cron-scripts
  (git-project
    (name "cron-scripts")
    (group "vds-admin")
    (output
      "/home/oleg/majordomo/vds-admin/cron-scripts")))

(define-public git-project-vds-check_vms_limit
  (git-project
    (name "check_vms_limit")
    (group "vds")
    (output
      "/home/oleg/majordomo/vds/check_vms_limit")))

(define-public git-project-vds-libguestfs
  (git-project
    (name "libguestfs")
    (group "vds")
    (output
      "/home/oleg/majordomo/vds/libguestfs")))

(define-public git-project-vds-vds
  (git-project
    (name "vds")
    (group "vds")
    (output
      "/home/oleg/majordomo/vds/vds")))

(define-public git-project-vds-vds-xenial
  (git-project
    (name "vds-xenial")
    (group "vds")
    (output
      "/home/oleg/majordomo/vds/vds-xenial")))

(define-public git-project-vds-vm-ispmanager
  (git-project
    (name "vm-ispmanager")
    (group "vds")
    (output
      "/home/oleg/majordomo/vds/vm-ispmanager")))

(define-public git-project-webservices-apache2-perl518
  (git-project
    (name "apache2-perl518")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/apache2-perl518")))

(define-public git-project-webservices-apache2-php44
  (git-project
    (name "apache2-php44")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/apache2-php44")))

(define-public git-project-webservices-apache2-php52
  (git-project
    (name "apache2-php52")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/apache2-php52")))

(define-public git-project-webservices-apache2-php53
  (git-project
    (name "apache2-php53")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/apache2-php53")))

(define-public git-project-webservices-apache2-php54
  (git-project
    (name "apache2-php54")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/apache2-php54")))

(define-public git-project-webservices-apache2-php55
  (git-project
    (name "apache2-php55")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/apache2-php55")))

(define-public git-project-webservices-apache2-php56
  (git-project
    (name "apache2-php56")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/apache2-php56")))

(define-public git-project-webservices-apache2-php70
  (git-project
    (name "apache2-php70")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/apache2-php70")))

(define-public git-project-webservices-apache2-php71
  (git-project
    (name "apache2-php71")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/apache2-php71")))

(define-public git-project-webservices-apache2-php72
  (git-project
    (name "apache2-php72")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/apache2-php72")))

(define-public git-project-webservices-apache2-php73
  (git-project
    (name "apache2-php73")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/apache2-php73")))

(define-public git-project-webservices-apache2-php73-personal
  (git-project
    (name "apache2-php73-personal")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/apache2-php73-personal")))

(define-public git-project-webservices-apache2-php74
  (git-project
    (name "apache2-php74")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/apache2-php74")))

(define-public git-project-webservices-apache2-php74-personal
  (git-project
    (name "apache2-php74-personal")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/apache2-php74-personal")))

(define-public git-project-webservices-apache2-php80
  (git-project
    (name "apache2-php80")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/apache2-php80")))

(define-public git-project-webservices-cron
  (git-project
    (name "cron")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/cron")))

(define-public git-project-webservices-ftpserver
  (git-project
    (name "ftpserver")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/ftpserver")))

(define-public git-project-webservices-http-fileserver
  (git-project
    (name "http-fileserver")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/http-fileserver")))

(define-public git-project-webservices-mariadb
  (git-project
    (name "mariadb")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/mariadb")))

(define-public git-project-webservices-memcached
  (git-project
    (name "memcached")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/memcached")))

(define-public git-project-webservices-nginx
  (git-project
    (name "nginx")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/nginx")))

(define-public git-project-webservices-nginx-php73-private
  (git-project
    (name "nginx-php73-private")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/nginx-php73-private")))

(define-public git-project-webservices-nodejs1213
  (git-project
    (name "nodejs1213")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/nodejs1213")))

(define-public git-project-webservices-parser-lebedeva.git
  (git-project
    (name "parser-lebedeva.git")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/parser-lebedeva.git")))

(define-public git-project-webservices-php4
  (git-project
    (name "php4")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/php4")))

(define-public git-project-webservices-php52
  (git-project
    (name "php52")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/php52")))

(define-public git-project-webservices-php52-extra
  (git-project
    (name "php52-extra")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/php52-extra")))

(define-public git-project-webservices-phpMyAdmin
  (git-project
    (name "phpMyAdmin")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/phpMyAdmin")))

(define-public git-project-webservices-postfix
  (git-project
    (name "postfix")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/postfix")))

(define-public git-project-webservices-postgresql
  (git-project
    (name "postgresql")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/postgresql")))

(define-public git-project-webservices-redis
  (git-project
    (name "redis")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/redis")))

(define-public git-project-webservices-rsyslog
  (git-project
    (name "rsyslog")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/rsyslog")))

(define-public git-project-webservices-ssh-guest-room
  (git-project
    (name "ssh-guest-room")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/ssh-guest-room")))

(define-public git-project-webservices-ssh-sup-room
  (git-project
    (name "ssh-sup-room")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/ssh-sup-room")))

(define-public git-project-webservices-ssh2docker-auth
  (git-project
    (name "ssh2docker-auth")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/ssh2docker-auth")))

(define-public git-project-webservices-uwsgi-python37
  (git-project
    (name "uwsgi-python37")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/uwsgi-python37")))

(define-public git-project-webservices-uwsgi-ruby
  (git-project
    (name "uwsgi-ruby")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/uwsgi-ruby")))

(define-public git-project-webservices-uwsgi-ruby26
  (git-project
    (name "uwsgi-ruby26")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/uwsgi-ruby26")))

(define-public git-project-webservices-vncproxy
  (git-project
    (name "vncproxy")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/vncproxy")))

(define-public git-project-webservices-vulnix
  (git-project
    (name "vulnix")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/vulnix")))

(define-public git-project-webservices-webftp
  (git-project
    (name "webftp")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/webftp")))

(define-public git-project-webservices-webftp-new
  (git-project
    (name "webftp-new")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/webftp-new")))

(define-public git-project-webservices-webssh
  (git-project
    (name "webssh")
    (group "webservices")
    (output
      "/home/oleg/majordomo/webservices/webssh")))

(define-public git-project-zdetovetskiy-nixos
  (git-project
    (name "nixos")
    (group "zdetovetskiy")
    (output
      "/home/oleg/majordomo/zdetovetskiy/nixos")))

(define github-missing-repositories
  (make-parameter '()))

(define missing-repositories
  (make-parameter '()))

(define (main args)
  (fold-projects (lambda (project projects)
                   (cons (if (file-exists? (git-project-output project))
                             (with-directory-excursion (git-project-output project)
                               (let* ((port (open-pipe* OPEN_READ "git" "remote" "-v"))
                                      (output (read-string port)))
                                 (close-port port)
                                 (let ((remote (string-append "git@github.com:6d6a/"
                                                              (git-project-group project)
                                                              "-"
                                                              (git-project-name project)
                                                              ".git")))
                                   (if (any identity
                                            (map (lambda (remote)
                                                   ;; TODO: Check for GitHub url in with "origin" name.
                                                   (match (string-split remote #\tab)
                                                     (("github" url)
                                                      #t)
                                                     (_ #f)))
                                                 (delete "" (string-split (string-trim-right output #\newline) #\newline))))
                                       (begin
                                         (format #t "~%~%@ ~a~%" (git-project-output project))
                                         (invoke "git" "remote" "set-url" "github" remote)
                                         (if (= (system* "sshpass" "-Ppassphrase" (string-append "-p" (pass "show" "github/ssh/id_rsa_github"))
                                                         "git" "fetch" "github")
                                                0)
                                             (display "succeeds to fetch github\n")
                                             ;; TODO: Check if branch is different
                                             ;; (system* "git" "branch" "github-master" "github/master")

                                             (github-missing-repositories (cons (git-project-output project) (github-missing-repositories)))))
                                       (invoke "git" "remote" "add" "github" remote)))))
                             (missing-repositories (cons (git-project-output project) (missing-repositories))))
                         projects))
                 '())
  (format #t "~%GitHub missing repositories:~%")
  (format #t "~{~a~%~}~%" (sort (github-missing-repositories) string<))
  (format #t "~%Missing repositories:~%")
  (format #t "~{~a~%~}~%" (sort (missing-repositories) string<)))

