'(("groups" .
   #((("rules"
       .
       #((("labels" ("severity" . "warning"))
          ("expr" . "exim_queue != 0")
          ("for" . "1h")
          ("annotations"
           ("summary" . "Exim non-empty queue")
           ("description" . "{{ $value }} messages in exim queue."))
          ("alert" . "EximQueue"))))
      ("name" . "exim-exporter")))))

