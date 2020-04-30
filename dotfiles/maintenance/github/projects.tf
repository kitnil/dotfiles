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

resource "github_repository" "bitlbee-libpurple" {
  name           = "bitlbee-libpurple"
  description    = "Fork https://github.com/ezkrg/docker-bitlbee-libpurple.git"
  default_branch = "wip-local"
}
