#!/usr/bin/env -S bash -c 'TF_VAR_GITHUB_TOKEN=$(pass github/tokens/personal/wigust/terraform) terraform apply'

variable "GITHUB_TOKEN" {
  type = string
}

provider "github" {
  organization = "kitnil"
  token        = var.GITHUB_TOKEN
  version      = "~> 2.3"
}
