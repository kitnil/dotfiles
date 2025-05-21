variable "KUBERNETES_PASSWORD" {
  type = string
}

resource "harbor_user" "kubernetes" {
  username = "kubernetes"
  password = var.KUBERNETES_PASSWORD
  full_name = "Kubernetes"
  email = "go.wigust@gmail.com"
}
