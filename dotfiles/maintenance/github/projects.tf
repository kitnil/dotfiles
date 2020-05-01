resource "github_repository" "dotfiles" {
  name           = "dotfiles"
  description    = "My dotfiles"
  default_branch = "master"
}

resource "github_repository" "guix" {
  name           = "guix"
  description    = "Fork https://git.savannah.gnu.org/cgit/guix.git"
  default_branch = "master"
  topics         = ["guix", "guile", "scheme"]
}

resource "github_repository" "guix-wigust" {
  name           = "guix-wigust"
  description    = "Extra packages for Guix"
  default_branch = "master"
  topics         = ["guix", "guile", "scheme"]
}

resource "github_repository" "bitlbee-libpurple" {
  name           = "bitlbee-libpurple"
  description    = "Fork https://github.com/ezkrg/docker-bitlbee-libpurple.git"
  default_branch = "wip-local"
}

resource "github_repository" "jenkins-shared-library" {
  name           = "jenkins-shared-library"
  default_branch = "master"
}

resource "github_repository" "awesome" {
  name           = "awesome"
  default_branch = "master"
}

resource "github_repository" "reevefresh" {
  name           = "reevefresh"
  description    = "Send notifications about Twitch online channels to Slack channel"
  default_branch = "master"
}

resource "github_repository" "nix-overlay-wigust" {
  name           = "nix-overlay-wigust"
  description    = "Nix overlay"
  default_branch = "master"
}

resource "github_repository" "tomegus" {
  name           = "tomegus"
  description    = "Rogue like game in C"
  default_branch = "master"
}

resource "github_repository" "emacs-org-mode" {
  name           = "emacs-org-mode"
  description    = "Fork git://orgmode.org/org-mode.git"
  default_branch = "master"
}

resource "github_repository" "nixpkgs" {
  name           = "nixpkgs"
  description    = "Fork https://github.com/NixOS/nixpkgs.git"
  default_branch = "master"
}

resource "github_repository" "multicd" {
  name           = "multicd"
  description    = "Fork https://github.com/IsaacSchemm/MultiCD"
  default_branch = "master"
}

resource "github_repository" "mikrotik-pxe" {
  name           = "mikrotik-pxe"
  description    = "Jenkinsfile for netboot.xyz.kpxe deploy to MikroTik"
  default_branch = "master"
}
