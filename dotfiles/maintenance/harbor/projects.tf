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
