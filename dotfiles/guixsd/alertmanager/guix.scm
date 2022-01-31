'(("groups" .
   #((("rules"
       .
       #((("labels" ("severity" . "warning"))
          ("for" . "1m")
          ("expr" . "count(sum(guix_channel) by (commit)) > 1")
          ("annotations"
           ("summary"
            .
            "Host Guix version deviations (instance {{ $labels.instance }})")
           ("description"
            .
            "Different Guix versions are running
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostGuixVersionDeviations"))
         ))
      ("name" . "guix-exporter")))))
