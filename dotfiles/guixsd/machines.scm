(list

 (build-machine
  (name "vm2.wugi.info")
  (systems '("x86_64-linux"))
  (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK4vfgz+tUUJFROCgTg+mRtHnr4yx1FeItXaAzmpP4lE root@(none)")
  (user "oleg")
  (private-key "/home/oleg/.ssh/id_rsa")
  (speed 2.0))

 (build-machine
  (name "mjru")
  (systems '("x86_64-linux"))
  (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICnqb74jWKmirhfbXKrt047ECzaSiCsZ4j3mYpTSbYEa root@gnu")
  (user "oleg")
  (private-key "/home/oleg/.ssh/id_rsa")
  (speed 2.0))

 (build-machine
  (name "spb")
  (systems '("x86_64-linux"))
  (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICpIN8RCMGlk+ckwq4Rx0+DHyjVh7dcWklqYZ0KBq1hk root@(none)")
  (user "oleg")
  (port 19022)
  (private-key "/home/oleg/.ssh/id_rsa")
  (speed 3.0))

 (build-machine
  (name "ci.intr")
  (systems '("x86_64-linux"))
  (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJcxtgcXJR11mAvqXLZCuQq3lgtv9pwHJeFyMRDun+OU root@ci.intr")
  (user "oleg")
  (port 17500)
  (private-key "/home/oleg/.ssh/id_rsa_workstation"))

)
