'(("groups" .
   #((("rules"
       .
       #((("labels" ("severity" . "critical"))
          ("expr" . "absent(exim_queue)")
          ("for" . "1m")
          ("annotations"
           ("summary" . "Absent exim queue")
           ("description" . "Absent exim queue."))
          ("alert" . "EximAbsent"))
         (("labels" ("severity" . "warning"))
          ("expr" . "exim_queue != 0")
          ("for" . "1h")
          ("annotations"
           ("summary" . "Exim non-empty queue")
           ("description" . "{{ $value }} messages in exim queue."))
          ("alert" . "EximQueue"))))
      ("name" . "exim-exporter")))))

