'(("groups"
   .
   #((("rules"
       .
       #((("labels" ("severity" . "critical"))
          ("expr"
           .
           "100 - lvm_lv_data_percent{lv_name=\"pool\"} < 10")
          ("annotations"
           ("summary"
            .
            "Logical Thin Volume has less than 10% space left.")
           ("description"
            .
            "Logical Thin Volume {{ $labels.lv_name }} at {{ $labels.instance }} has only {{ printf \"%.2f\" $value }}% available space left."))
          ("alert" . "LvmLvDataAlmostOutOfSpace"))
         (("labels" ("severity" . "critical"))
          ("for" . "10m")
          ("expr"
           .
           "absent(lvm_lv_data_percent{lv_name=\"pool\"})")
          ("annotations"
           ("summary"
            .
            "Absent metrics for Logical Thin Volume.")
           ("description"
            .
            "Absent metrics for Logical Thin Volume at {{ $labels.instance }}."))
          ("alert" . "LvmLvDataPercentAbsent"))))
      ("name" . "lvm")))))
