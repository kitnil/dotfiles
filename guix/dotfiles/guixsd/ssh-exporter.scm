'(("groups" .
   #((("rules"
       .
       #((("labels" ("severity" . "warning"))
          ("expr" . "ssh_success != 1")
          ("annotations"
           ("summary" . "SSH connection failure")
           ("description" . "SSH connection failure."))
          ("alert" . "SshFailure"))))
      ("name" . "ssh")))))
