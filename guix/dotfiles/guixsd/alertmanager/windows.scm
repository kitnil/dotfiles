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
         (("labels" ("severity" . "warning"))
          ("for" . "1h")
          ("expr"
           .
           "windows_logical_disk_free_bytes / windows_logical_disk_size_bytes * 100 < 5")
          ("annotations"
           ("summary"
            .
            "Filesystem has less than 5% space left.")
           ("description"
            .
            "Filesystem on {{ $labels.volume }} at {{ $labels.instance }} has only {{ printf \"%.2f\" $value }}% available space left."))
          ("alert" . "WindowsFilesystemAlmostOutOfSpace"))))
      ("name" . "windows-exporter")))))
