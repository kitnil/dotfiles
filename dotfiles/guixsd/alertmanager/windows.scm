'(("groups" .
   #((("rules"
       .
       #((("labels" ("severity" . "warning"))
          ("for" . "1m")
          ("expr" . "windows_exporter_collector_success != 1")
          ("annotations"
           ("summary"
            .
            "Windows exporter collector {{ $labels.collector }} failed (instance {{ $labels.instance }})"))
          ("alert" . "WindowsCollectorFail"))
         ))
      ("name" . "windows-exporter")))))
