'(("groups" .
   #((("rules"
       .
       #((("labels" ("severity" . "warning"))
          ("for" . "1m")
          ("expr" . "shepherd_service_status < 1")
          ("annotations"
           ("summary"
            .
            "{{ $labels.service }} stopped on (instance {{ $labels.instance }})"))
          ("alert" . "ShepherdServiceStopped"))))
      ("name" . "shepherd-exporter")))))
