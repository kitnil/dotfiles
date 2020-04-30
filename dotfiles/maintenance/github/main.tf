variable "GITHUB_TOKEN" {
  type = string
}

provider "github" {
  organization = "kitnil"
  token        = var.GITHUB_TOKEN
  version      = "~> 2.3"
}
