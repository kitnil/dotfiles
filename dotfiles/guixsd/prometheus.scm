'(("groups"
   .
   #((("rules"
       .
       #((("labels" ("severity" . "critical"))
          ("expr" . "up != 1")
          ("annotations"
           ("summary"
            .
            "Exporter for {{ $labels.job }} job is down on {{ $labels.instance }}.")
           ("runbook_url"
            .
            "https://prometheus.io/docs/instrumenting/exporters/")
           ("description"
            .
            "Exporter failed\n  VALUE = {{ $value }}\n  LABELS = {{ $labels }}"))
          ("alert" . "PrometheusJobDown"))))
      ("name" . "prometheus")))))
