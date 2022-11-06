'(("groups" .
   #((("rules"
       .
       #((("labels" ("severity" . "critical"))
          ("expr" . "absent(smartctl_device_attribute{attribute_id=\"5\", attribute_value_type=\"raw\"})")
          ("for" . "20m")
          ("annotations"
           ("summary" . "Absent smartctl reallocated sectors counts")
           ("description" . "Absent smartctl reallocated sectors counts."))
          ("alert" . "DiskAbsentReallocatedSectors"))
         (("labels"
           ("severity" . "critical"))
          ("expr"
           .
           "deriv(smartctl_device_attribute{attribute_id=\"5\",attribute_value_type=\"raw\"}[15m]) > 0")
          ("annotations"
           ("summary" . "Reallocated sectors detected.")
           ("description" . "Disk {{ $labels.model_name }} has {{ $value }} reallocated sectors."))
          ("alert" . "DiskReallocatedSectors"))))
      ("name" . "smartctl-exporter")))))
