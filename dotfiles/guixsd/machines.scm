(list

 (build-machine
  (name "vm1.wugi.info")
  (systems '("x86_64-linux"))
  (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICnqb74jWKmirhfbXKrt047ECzaSiCsZ4j3mYpTSbYEa root@gnu")
  (user "oleg")
  (private-key "/home/oleg/.ssh/id_rsa")
  (speed 2.0))

 (build-machine
  (name "vm2.wugi.info")
  (systems '("x86_64-linux"))
  (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK4vfgz+tUUJFROCgTg+mRtHnr4yx1FeItXaAzmpP4lE root@(none)")
  (user "oleg")
  (private-key "/home/oleg/.ssh/id_rsa")
  (speed 2.0))

 (build-machine
  (name "vm3.wugi.info")
  (systems '("x86_64-linux"))
  (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICnqb74jWKmirhfbXKrt047ECzaSiCsZ4j3mYpTSbYEa root@gnu")
  (user "oleg")
  (private-key "/home/oleg/.ssh/id_rsa")
  (speed 2.0))

)
