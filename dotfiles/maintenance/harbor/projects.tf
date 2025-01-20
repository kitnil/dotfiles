resource "harbor_project" "dockerhub" {
  name                   = "dockerhub"
  registry_id            = harbor_registry.dockerhub.registry_id
  vulnerability_scanning = false
  public                 = true
  force_destroy          = true
}

resource "harbor_registry" "dockerhub" {
  provider_name = "docker-hub"
  name          = "dockerhub"
  endpoint_url  = "https://hub.docker.com"
}

resource "harbor_project" "nixos" {
  name = "nixos"
  public = true
  vulnerability_scanning = false
}

resource "harbor_retention_policy" "nixos" {
  scope = harbor_project.nixos.id
  schedule = "Daily"
  rule {
    most_recently_pulled = 1
    repo_matching = "**"
  }
  rule {
    most_recently_pushed = 2
    repo_matching = "**"
  }
}

resource "harbor_project" "openwrt" {
  name = "openwrt"
  public = true
  vulnerability_scanning = false
}

resource "harbor_project_member_user" "kubernetes_openwrt" {
  project_id = harbor_project.openwrt.id
  user_name = "kubernetes"
  role = "developer"
}
