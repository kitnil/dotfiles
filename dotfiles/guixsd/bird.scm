'(("groups" .
   #((("rules"
       .
       #((("labels"
           ("severity" . "critical"))
          ("expr"
           .
           "sum by (instance,import_filter,proto) (bird_protocol_prefix_import_count{proto=\"BGP\"}) == 0")
          ("annotations"
           ("summary" . "Bird No Imports")
           ("details"
            .
            "{{ $value }} prefixes imported totally")
           ("description"
            .
            "All {{ $labels.proto }} sessions are unused! External connectivity affected"))
          ("alert" . "BirdNoImports"))))
      ("name" . "bird-exporter")))))
