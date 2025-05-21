variable "HARBOR_URL" {
  type = string
}

variable "HARBOR_USERNAME" {
  type = string
}

variable "HARBOR_PASSWORD" {
  type = string
}

terraform {
  required_version = ">= 0.14.0"
  required_providers {
    harbor = {
      source = "goharbor/harbor"
      version = "3.10.8"
    }
  }
}

provider "harbor" {
  url      = var.HARBOR_URL
  username = var.HARBOR_USERNAME
  password = var.HARBOR_PASSWORD
}
