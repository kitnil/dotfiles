resource "harbor_garbage_collection" "daily" {
  schedule        = "Daily"
  delete_untagged = true
}
