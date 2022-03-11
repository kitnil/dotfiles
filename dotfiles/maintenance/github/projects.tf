resource "github_repository" "dotfiles" {
  name           = "dotfiles"
  description    = "My dotfiles"
  default_branch = "master"
  topics         = ["emacs", "guix", "guile", "nix", "scheme", "shell", "stumpwm"]
}

resource "github_repository" "notes" {
  name           = "notes"
  description    = "My notes"
  default_branch = "master"
}

resource "github_repository_deploy_key" "deploy-key-notes" {
  title      = "gitlab.com"
  repository = "notes"
  key        = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC80Rg7NwH5FWZZ9BbritO3xWG/E413UjfXowBgIhspYVC7vKguzA2ifhXJuUgPI6/8oP1ESfIrd6gUNkA0lJOkbco+CAHJFaFmHUyMxF9d6+KNDQFmiRM8+X0RgoD1GeNgGARHYutAkhhUcwFmHzW8ZrAk/pNYc/2+1dVlUV7jLx5WuYClOr14VZ7yOW+cWcmSbXdzwSW3jqUOTABKmqZMm2dfH8OtD6M8RB7bh/h41Gat5HwyyzVNPXDvuTt8+gUDrGU/xYS0/c2Inia11m+/HsEToDgk0Vxz+kj6TnN9Zs+JhzOH4dqnEvlYbRffACF4x6t09Mb+G0G/oBqYbeU2kTr6RrCn0+DoFqvDC63hMVq8F18/FTLTiuyN5Wqr278xGYKssIFivKe/pbLOiKob1wtKyrDkozOaA4Bpw3ndUddGNNo5K+j5kMDzXoVvEdj89gxW2RhYl9lw8zO+sD3d9Bbfh7S9Ipv9hyuZEoaggHAyx4o6vSfGg9Kci/ugCXKECO6iP6kwNZAtaGemm6cwQnK0GcUtMNyEbo010/lj89zNsMRAUkSzyDeq89rjgTp+OuOSfBdGYUcsHRX7u6ab4wjHZOPwdatFjRieVECnRIArYkEacVMAbX5kbKEMpSvRUeoVot7Ky8+Y7bHBIt37A3ZfyxQGENtj769wE1U4Qw== git@gitlab.com"
  read_only  = "false"
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

resource "github_repository" "guix-artwork" {
  name           = "guix-artwork"
  description    = "Fork https://git.savannah.nongnu.org/cgit/guix/guix-artwork.git"
  default_branch = "slim-input-panel"
  topics         = ["guix", "guile", "scheme"]
}

resource "github_repository" "guix-misc" {
  name           = "guix-misc"
  description    = "Miscellaneous addons for Guix"
  default_branch = "master"
  topics         = ["guix", "guile", "scheme"]
}

resource "github_repository" "guix-wigust-services" {
  name           = "guix-wigust-services"
  description    = "Extra services for Guix"
  default_branch = "master"
  topics         = ["guix", "guile", "scheme"]
}

resource "github_repository" "guix-linux-nonfree" {
  name           = "guix-linux-nonfree"
  description    = "Non-free Linux kernel modules for Guix"
  default_branch = "master"
  topics         = ["guix", "guile", "scheme"]
}

resource "github_repository" "guix-nonfree" {
  name           = "guix-nonfree"
  description    = "Extra non-free packages for Guix"
  default_branch = "master"
  topics         = ["guix", "guile", "scheme"]
}

resource "github_repository" "guix-docker" {
  name           = "guix-docker"
  description    = "Docker image build by Guix with guix-daemon"
  default_branch = "master"
  topics         = ["guix", "guile", "scheme"]
}

resource "github_repository" "guix-door-kickers" {
  name           = "guix-door-kickers"
  description    = "Guix package for Door Kickers game"
  default_branch = "master"
  topics         = ["guix", "guile", "scheme"]
}

resource "github_repository" "guix-tome4" {
  name           = "guix-tome4"
  description    = "Guix package for Tales of Maj'Eyal rogue like game"
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

resource "github_repository" "yeastizzy" {
  name           = "yeastizzy"
  description    = "Send notifications about YouTube online channels to Slack channel"
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

resource "github_repository" "emacs-dorg" {
  name           = "emacs-dorg"
  description    = "Update Org-mode buffer according to Debbugs"
  default_branch = "master"
  topics         = ["emacs", "org", "debbug"]
}

resource "github_repository" "emacs-org-mode" {
  name           = "emacs-org-mode"
  description    = "Fork git://orgmode.org/org-mode.git"
  default_branch = "pdfview"
}

resource "github_repository" "emacs-tramp-auto-auth" {
  name           = "emacs-tramp-auto-auth"
  default_branch = "wip"
}

resource "github_repository" "emacs-vterm-toggle" {
  name           = "emacs-vterm-toggle"
  description    = "Fork https://github.com/jixiuf/vterm-toggle.git"
  default_branch = "wip"
}

resource "github_repository" "emacs-wi-utils" {
  name           = "emacs-wi-utils"
  description    = "Project and server management with Emacs"
  default_branch = "master"
}

resource "github_repository" "firefox-passff" {
  name           = "firefox-passff"
  description    = "Fork https://github.com/passff/passff.git"
  default_branch = "wip"
}

resource "github_repository" "firefox-org-capture-extension" {
  name           = "firefox-org-capture-extension"
  description    = "Fork https://github.com/sprig/org-capture-extension"
  default_branch = "wip-local"
}

resource "github_repository" "firefox-tab-slideshow" {
  name           = "firefox-tab-slideshow"
  description    = "Fork https://addons.mozilla.org/en-US/firefox/addon/tab-slideshow-we/"
  default_branch = "master"
}

resource "github_repository" "python-nmap2json" {
  name           = "python-nmap2json"
  description    = "Convert nmap output to JSON"
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
  default_branch = "wip"
}

resource "github_repository" "mikrotik-pxe" {
  name           = "mikrotik-pxe"
  description    = "Jenkinsfile for netboot.xyz.kpxe deploy to MikroTik"
  default_branch = "master"
}

resource "github_repository" "docker-wigust" {
  name           = "docker-wigust"
  description    = "Miscellaneous dockerfiles"
  default_branch = "master"
}

resource "github_repository" "backup-restic" {
  name           = "backup-restic"
  description    = "Jenkinsfile for backup with Restic"
  default_branch = "master"
}

resource "github_repository" "guix-zabbix" {
  name           = "guix-zabbix"
  description    = "Monitor Guix with Zabbix"
  default_branch = "master"
}

resource "github_repository" "wrapper-xclip" {
  name           = "wrapper-xclip"
  description    = "xclip wrapper"
  default_branch = "master"
}

resource "github_repository" "nix-docker-ipmi" {
  name           = "nix-docker-ipmi"
  description    = "Nix expression to build Bash script and container to connect to servers via IPMI"
  default_branch = "master"
}

resource "github_repository" "nix-ipmiview" {
  name           = "nix-ipmiview"
  description    = "Nix expression to build IPMIView to run via steam-run"
  default_branch = "master"
}

resource "github_repository" "filestash" {
  name           = "filestash"
  description    = "Fork https://github.com/mickael-kerjean/filestash"
  default_branch = "master"
}

resource "github_repository" "nix-flake-lock-update-plugin" {
  name           = "nix-flake-lock-update-plugin"
}
