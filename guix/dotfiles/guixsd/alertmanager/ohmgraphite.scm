'(("groups" .
   #((("rules"
       .
       #((("labels" ("severity" . "warning"))
          ("for" . "5m")
          ("expr" . "ohm_gpuati_celsius > 75")
          ("annotations"
           ("summary"
            .
            "Host physical component too hot (hostname {{ $labels.hostname }})")
           ("description"
            .
            "Physical hardware component too hot
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "OhmgraphitePhysicalComponentTooHot"))
         ))
      ("name" . "ohmgraphite")))))
