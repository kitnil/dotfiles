'(("groups" .
   #((("rules"
       .
       #((("labels" ("severity" . "warning"))
          ("for" . "1h")
          ("expr"
           .
           "(
  node_filesystem_avail_bytes{job=\"node\",fstype!=\"\"} / node_filesystem_size_bytes{job=\"node\",fstype!=\"\"} * 100 < 40
and
  predict_linear(node_filesystem_avail_bytes{job=\"node\",fstype!=\"\"}[6h], 24*60*60) < 0
and
  node_filesystem_readonly{job=\"node\",fstype!=\"\"} == 0
)
")
          ("annotations"
           ("summary"
            .
            "Filesystem is predicted to run out of space within the next 24 hours.")
           ("description"
            .
            "Filesystem on {{ $labels.device }} at {{ $labels.instance }} has only {{ printf \"%.2f\" $value }}% available space left and is filling up."))
          ("alert" . "NodeFilesystemSpaceFillingUp"))
         (("labels" ("severity" . "critical"))
          ("for" . "1h")
          ("expr"
           .
           "(
  node_filesystem_avail_bytes{job=\"node\",fstype!=\"\"} / node_filesystem_size_bytes{job=\"node\",fstype!=\"\"} * 100 < 20
and
  predict_linear(node_filesystem_avail_bytes{job=\"node\",fstype!=\"\"}[6h], 4*60*60) < 0
and
  node_filesystem_readonly{job=\"node\",fstype!=\"\"} == 0
)
")
          ("annotations"
           ("summary"
            .
            "Filesystem is predicted to run out of space within the next 4 hours.")
           ("description"
            .
            "Filesystem on {{ $labels.device }} at {{ $labels.instance }} has only {{ printf \"%.2f\" $value }}% available space left and is filling up fast."))
          ("alert" . "NodeFilesystemSpaceFillingUp"))
         (("labels" ("severity" . "warning"))
          ("for" . "1h")
          ("expr"
           .
           "(
  node_filesystem_avail_bytes{job=\"node\",fstype!=\"\"} / node_filesystem_size_bytes{job=\"node\",fstype!=\"\"} * 100 < 5
and
  node_filesystem_readonly{job=\"node\",fstype!=\"\"} == 0
)
")
          ("annotations"
           ("summary"
            .
            "Filesystem has less than 5% space left.")
           ("description"
            .
            "Filesystem on {{ $labels.device }} at {{ $labels.instance }} has only {{ printf \"%.2f\" $value }}% available space left."))
          ("alert" . "NodeFilesystemAlmostOutOfSpace"))
         (("labels" ("severity" . "critical"))
          ("for" . "1h")
          ("expr"
           .
           "(
  node_filesystem_avail_bytes{job=\"node\",fstype!=\"\"} / node_filesystem_size_bytes{job=\"node\",fstype!=\"\"} * 100 < 3
and
  node_filesystem_readonly{job=\"node\",fstype!=\"\"} == 0
)
")
          ("annotations"
           ("summary"
            .
            "Filesystem has less than 3% space left.")
           ("description"
            .
            "Filesystem on {{ $labels.device }} at {{ $labels.instance }} has only {{ printf \"%.2f\" $value }}% available space left."))
          ("alert" . "NodeFilesystemAlmostOutOfSpace"))
         (("labels" ("severity" . "warning"))
          ("for" . "1h")
          ("expr"
           .
           "(
  node_filesystem_files_free{job=\"node\",fstype!=\"\"} / node_filesystem_files{job=\"node\",fstype!=\"\"} * 100 < 40
and
  predict_linear(node_filesystem_files_free{job=\"node\",fstype!=\"\"}[6h], 24*60*60) < 0
and
  node_filesystem_readonly{job=\"node\",fstype!=\"\"} == 0
)
")
          ("annotations"
           ("summary"
            .
            "Filesystem is predicted to run out of inodes within the next 24 hours.")
           ("description"
            .
            "Filesystem on {{ $labels.device }} at {{ $labels.instance }} has only {{ printf \"%.2f\" $value }}% available inodes left and is filling up."))
          ("alert" . "NodeFilesystemFilesFillingUp"))
         (("labels" ("severity" . "critical"))
          ("for" . "1h")
          ("expr"
           .
           "(
  node_filesystem_files_free{job=\"node\",fstype!=\"\"} / node_filesystem_files{job=\"node\",fstype!=\"\"} * 100 < 20
and
  predict_linear(node_filesystem_files_free{job=\"node\",fstype!=\"\"}[6h], 4*60*60) < 0
and
  node_filesystem_readonly{job=\"node\",fstype!=\"\"} == 0
)
")
          ("annotations"
           ("summary"
            .
            "Filesystem is predicted to run out of inodes within the next 4 hours.")
           ("description"
            .
            "Filesystem on {{ $labels.device }} at {{ $labels.instance }} has only {{ printf \"%.2f\" $value }}% available inodes left and is filling up fast."))
          ("alert" . "NodeFilesystemFilesFillingUp"))
         (("labels" ("severity" . "warning"))
          ("for" . "1h")
          ("expr"
           .
           "(
  node_filesystem_files_free{job=\"node\",fstype!=\"\"} / node_filesystem_files{job=\"node\",fstype!=\"\"} * 100 < 5
and
  node_filesystem_readonly{job=\"node\",fstype!=\"\"} == 0
)
")
          ("annotations"
           ("summary"
            .
            "Filesystem has less than 5% inodes left.")
           ("description"
            .
            "Filesystem on {{ $labels.device }} at {{ $labels.instance }} has only {{ printf \"%.2f\" $value }}% available inodes left."))
          ("alert" . "NodeFilesystemAlmostOutOfFiles"))
         (("labels" ("severity" . "critical"))
          ("for" . "1h")
          ("expr"
           .
           "(
  node_filesystem_files_free{job=\"node\",fstype!=\"\"} / node_filesystem_files{job=\"node\",fstype!=\"\"} * 100 < 3
and
  node_filesystem_readonly{job=\"node\",fstype!=\"\"} == 0
)
")
          ("annotations"
           ("summary"
            .
            "Filesystem has less than 3% inodes left.")
           ("description"
            .
            "Filesystem on {{ $labels.device }} at {{ $labels.instance }} has only {{ printf \"%.2f\" $value }}% available inodes left."))
          ("alert" . "NodeFilesystemAlmostOutOfFiles"))
         (("labels" ("severity" . "warning"))
          ("for" . "1h")
          ("expr"
           .
           "rate(node_network_receive_errs_total[2m]) / rate(node_network_receive_packets_total[2m]) > 0.01
")
          ("annotations"
           ("summary"
            .
            "Network interface is reporting many receive errors.")
           ("description"
            .
            "{{ $labels.instance }} interface {{ $labels.device }} has encountered {{ printf \"%.0f\" $value }} receive errors in the last two minutes."))
          ("alert" . "NodeNetworkReceiveErrs"))
         (("labels" ("severity" . "warning"))
          ("for" . "1h")
          ("expr"
           .
           "rate(node_network_transmit_errs_total[2m]) / rate(node_network_transmit_packets_total[2m]) > 0.01
")
          ("annotations"
           ("summary"
            .
            "Network interface is reporting many transmit errors.")
           ("description"
            .
            "{{ $labels.instance }} interface {{ $labels.device }} has encountered {{ printf \"%.0f\" $value }} transmit errors in the last two minutes."))
          ("alert" . "NodeNetworkTransmitErrs"))
         (("labels" ("severity" . "warning"))
          ("for" . null)
          ("expr"
           .
           "(node_nf_conntrack_entries / node_nf_conntrack_entries_limit) > 0.75
")
          ("annotations"
           ("summary"
            .
            "Number of conntrack are getting close to the limit.")
           ("description"
            .
            "{{ $value | humanizePercentage }} of conntrack entries are used."))
          ("alert" . "NodeHighNumberConntrackEntriesUsed"))
         (("labels" ("severity" . "warning"))
          ("for" . null)
          ("expr"
           .
           "node_textfile_scrape_error{job=\"node\"} == 1
")
          ("annotations"
           ("summary"
            .
            "Node Exporter text file collector failed to scrape.")
           ("description"
            .
            "Node Exporter text file collector failed to scrape."))
          ("alert" . "NodeTextFileCollectorScrapeError"))
         (("labels" ("severity" . "warning"))
          ("for" . "10m")
          ("expr"
           .
           "(
  node_timex_offset_seconds > 0.05
and
  deriv(node_timex_offset_seconds[5m]) >= 0
)
or
(
  node_timex_offset_seconds < -0.05
and
  deriv(node_timex_offset_seconds[5m]) <= 0
)
")
          ("annotations"
           ("summary" . "Clock skew detected.")
           ("description"
            .
            "Clock on {{ $labels.instance }} is out of sync by more than 300s. Ensure NTP is configured correctly on this host."))
          ("alert" . "NodeClockSkewDetected"))
         (("labels" ("severity" . "warning"))
          ("for" . "10m")
          ("expr"
           .
           "min_over_time(node_timex_sync_status[5m]) == 0
and
node_timex_maxerror_seconds >= 16
")
          ("annotations"
           ("summary" . "Clock not synchronising.")
           ("description"
            .
            "Clock on {{ $labels.instance }} is not synchronising. Ensure NTP is configured on this host."))
          ("alert" . "NodeClockNotSynchronising"))
         (("labels" ("severity" . "warning"))
          ("for" . "2m")
          ("expr"
           .
           "node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes * 100 < 10")
          ("annotations"
           ("summary"
            .
            "Host out of memory (instance {{ $labels.instance }})")
           ("description"
            .
            "Node memory is filling up (< 10% left)
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostOutOfMemory"))
         (("labels" ("severity" . "warning"))
          ("for" . "2m")
          ("expr"
           .
           "rate(node_vmstat_pgmajfault[1m]) > 1000")
          ("annotations"
           ("summary"
            .
            "Host memory under memory pressure (instance {{ $labels.instance }})")
           ("description"
            .
            "The node is under heavy memory pressure. High rate of major page faults
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostMemoryUnderMemoryPressure"))
         (("labels" ("severity" . "warning"))
          ("for" . "5m")
          ("expr"
           .
           "sum by (instance) (rate(node_network_receive_bytes_total[2m])) / 1024 / 1024 > 100")
          ("annotations"
           ("summary"
            .
            "Host unusual network throughput in (instance {{ $labels.instance }})")
           ("description"
            .
            "Host network interfaces are probably receiving too much data (> 100 MB/s)
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostUnusualNetworkThroughputIn"))
         (("labels" ("severity" . "warning"))
          ("for" . "5m")
          ("expr"
           .
           "sum by (instance) (rate(node_network_transmit_bytes_total[2m])) / 1024 / 1024 > 100")
          ("annotations"
           ("summary"
            .
            "Host unusual network throughput out (instance {{ $labels.instance }})")
           ("description"
            .
            "Host network interfaces are probably sending too much data (> 100 MB/s)
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostUnusualNetworkThroughputOut"))
         (("labels" ("severity" . "warning"))
          ("for" . "5m")
          ("expr"
           .
           "sum by (instance) (rate(node_disk_read_bytes_total[2m])) / 1024 / 1024 > 50")
          ("annotations"
           ("summary"
            .
            "Host unusual disk read rate (instance {{ $labels.instance }})")
           ("description"
            .
            "Disk is probably reading too much data (> 50 MB/s)
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostUnusualDiskReadRate"))
         (("labels" ("severity" . "warning"))
          ("for" . "2m")
          ("expr"
           .
           "sum by (instance) (rate(node_disk_written_bytes_total[2m])) / 1024 / 1024 > 50")
          ("annotations"
           ("summary"
            .
            "Host unusual disk write rate (instance {{ $labels.instance }})")
           ("description"
            .
            "Disk is probably writing too much data (> 50 MB/s)
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostUnusualDiskWriteRate"))
         (("labels" ("severity" . "warning"))
          ("for" . "2m")
          ("expr"
           .
           "(node_filesystem_avail_bytes * 100) / node_filesystem_size_bytes < 10 and ON (instance, device, mountpoint) node_filesystem_readonly == 0")
          ("annotations"
           ("summary"
            .
            "Host out of disk space (instance {{ $labels.instance }})")
           ("description"
            .
            "Disk is almost full (< 10% left)
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostOutOfDiskSpace"))
         (("labels" ("severity" . "warning"))
          ("for" . "2m")
          ("expr"
           .
           "(node_filesystem_avail_bytes * 100) / node_filesystem_size_bytes < 10 and ON (instance, device, mountpoint) predict_linear(node_filesystem_avail_bytes{fstype!~\"tmpfs\"}[1h], 24 * 3600) < 0 and ON (instance, device, mountpoint) node_filesystem_readonly == 0")
          ("annotations"
           ("summary"
            .
            "Host disk will fill in 24 hours (instance {{ $labels.instance }})")
           ("description"
            .
            "Filesystem is predicted to run out of space within the next 24 hours at current write rate
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostDiskWillFillIn24Hours"))
         (("labels" ("severity" . "warning"))
          ("for" . "2m")
          ("expr"
           .
           "node_filesystem_files_free{mountpoint =\"/rootfs\"} / node_filesystem_files{mountpoint=\"/rootfs\"} * 100 < 10 and ON (instance, device, mountpoint) node_filesystem_readonly{mountpoint=\"/rootfs\"} == 0")
          ("annotations"
           ("summary"
            .
            "Host out of inodes (instance {{ $labels.instance }})")
           ("description"
            .
            "Disk is almost running out of available inodes (< 10% left)
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostOutOfInodes"))
         (("labels" ("severity" . "warning"))
          ("for" . "2m")
          ("expr"
           .
           "node_filesystem_files_free{mountpoint =\"/rootfs\"} / node_filesystem_files{mountpoint=\"/rootfs\"} * 100 < 10 and predict_linear(node_filesystem_files_free{mountpoint=\"/rootfs\"}[1h], 24 * 3600) < 0 and ON (instance, device, mountpoint) node_filesystem_readonly{mountpoint=\"/rootfs\"} == 0")
          ("annotations"
           ("summary"
            .
            "Host inodes will fill in 24 hours (instance {{ $labels.instance }})")
           ("description"
            .
            "Filesystem is predicted to run out of inodes within the next 24 hours at current write rate
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostInodesWillFillIn24Hours"))
         (("labels" ("severity" . "warning"))
          ("for" . "2m")
          ("expr"
           .
           "rate(node_disk_read_time_seconds_total[1m]) / rate(node_disk_reads_completed_total[1m]) > 0.1 and rate(node_disk_reads_completed_total[1m]) > 0")
          ("annotations"
           ("summary"
            .
            "Host unusual disk read latency (instance {{ $labels.instance }})")
           ("description"
            .
            "Disk latency is growing (read operations > 100ms)
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostUnusualDiskReadLatency"))
         (("labels" ("severity" . "warning"))
          ("for" . "2m")
          ("expr"
           .
           "rate(node_disk_write_time_seconds_total[1m]) / rate(node_disk_writes_completed_total[1m]) > 0.1 and rate(node_disk_writes_completed_total[1m]) > 0")
          ("annotations"
           ("summary"
            .
            "Host unusual disk write latency (instance {{ $labels.instance }})")
           ("description"
            .
            "Disk latency is growing (write operations > 100ms)
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostUnusualDiskWriteLatency"))
         (("labels" ("severity" . "warning"))
          ("for" . "0m")
          ("expr"
           .
           "100 - (avg by(instance) (rate(node_cpu_seconds_total{mode=\"idle\"}[2m])) * 100) > 80")
          ("annotations"
           ("summary"
            .
            "Host high CPU load (instance {{ $labels.instance }})")
           ("description"
            .
            "CPU load is > 80%
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostHighCpuLoad"))
         (("labels" ("severity" . "warning"))
          ("for" . "0m")
          ("expr"
           .
           "avg by(instance) (rate(node_cpu_seconds_total{mode=\"steal\"}[5m])) * 100 > 10")
          ("annotations"
           ("summary"
            .
            "Host CPU steal noisy neighbor (instance {{ $labels.instance }})")
           ("description"
            .
            "CPU steal is > 10%. A noisy neighbor is killing VM performances or a spot instance may be out of credit.
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostCpuStealNoisyNeighbor"))
         (("labels" ("severity" . "warning"))
          ("for" . "0m")
          ("expr"
           .
           "(rate(node_context_switches_total{instance!=\"127.0.0.1:9100\"}[5m])) / (count without(cpu, mode) (node_cpu_seconds_total{mode=\"idle\"})) > 1000")
          ("annotations"
           ("summary"
            .
            "Host context switching (instance {{ $labels.instance }})")
           ("description"
            .
            "Context switching is growing on node (> 1000 / s)
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostContextSwitching"))
         (("labels" ("severity" . "warning"))
          ("for" . "2m")
          ("expr"
           .
           "(1 - (node_memory_SwapFree_bytes / node_memory_SwapTotal_bytes)) * 100 > 80")
          ("annotations"
           ("summary"
            .
            "Host swap is filling up (instance {{ $labels.instance }})")
           ("description"
            .
            "Swap is filling up (>80%)
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostSwapIsFillingUp"))
         (("labels" ("severity" . "warning"))
          ("for" . "0m")
          ("expr"
           .
           "node_systemd_unit_state{state=\"failed\"} == 1")
          ("annotations"
           ("summary"
            .
            "Host systemd service crashed (instance {{ $labels.instance }})")
           ("description"
            .
            "systemd service crashed
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostSystemdServiceCrashed"))
         (("labels" ("severity" . "warning"))
          ("for" . "5m")
          ("expr" . "node_hwmon_temp_celsius > 75")
          ("annotations"
           ("summary"
            .
            "Host physical component too hot (instance {{ $labels.instance }})")
           ("description"
            .
            "Physical hardware component too hot
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostPhysicalComponentTooHot"))
         (("labels" ("severity" . "critical"))
          ("for" . "0m")
          ("expr"
           .
           "node_hwmon_temp_crit_alarm_celsius == 1")
          ("annotations"
           ("summary"
            .
            "Host node overtemperature alarm (instance {{ $labels.instance }})")
           ("description"
            .
            "Physical node temperature alarm triggered
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostNodeOvertemperatureAlarm"))
         (("labels" ("severity" . "critical"))
          ("for" . "0m")
          ("expr"
           .
           "node_md_state{state=\"inactive\"} > 0")
          ("annotations"
           ("summary"
            .
            "Host RAID array got inactive (instance {{ $labels.instance }})")
           ("description"
            .
            "RAID array {{ $labels.device }} is in degraded state due to one or more disks failures. Number of spare drives is insufficient to fix issue automatically.
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostRaidArrayGotInactive"))
         (("labels" ("severity" . "warning"))
          ("for" . "2m")
          ("expr" . "node_md_disks{state=\"failed\"} > 0")
          ("annotations"
           ("summary"
            .
            "Host RAID disk failure (instance {{ $labels.instance }})")
           ("description"
            .
            "At least one device in RAID array on {{ $labels.instance }} failed. Array {{ $labels.md_device }} needs attention and possibly a disk swap
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostRaidDiskFailure"))
         (("labels" ("severity" . "warning"))
          ("for" . "6h")
          ("expr"
           .
           "count(sum(label_replace(node_uname_info{instance!=\"127.0.0.1:9100\"}, \"kernel\", \"$1\", \"release\", \"([0-9]+.[0-9]+.[0-9]+).*\")) by (kernel)) > 1")
          ("annotations"
           ("summary"
            .
            "Host kernel version deviations (instance {{ $labels.instance }})")
           ("description"
            .
            "Different kernel versions are running
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostKernelVersionDeviations"))
         (("labels" ("severity" . "warning"))
          ("for" . "0m")
          ("expr"
           .
           "increase(node_vmstat_oom_kill[1m]) > 0")
          ("annotations"
           ("summary"
            .
            "Host OOM kill detected (instance {{ $labels.instance }})")
           ("description"
            .
            "OOM kill detected
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostOomKillDetected"))
         (("labels" ("severity" . "info"))
          ("for" . "0m")
          ("expr"
           .
           "increase(node_edac_correctable_errors_total[1m]) > 0")
          ("annotations"
           ("summary"
            .
            "Host EDAC Correctable Errors detected (instance {{ $labels.instance }})")
           ("description"
            .
            "Host {{ $labels.instance }} has had {{ printf \"%.0f\" $value }} correctable memory errors reported by EDAC in the last 5 minutes.
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostEdacCorrectableErrorsDetected"))
         (("labels" ("severity" . "warning"))
          ("for" . "0m")
          ("expr"
           .
           "node_edac_uncorrectable_errors_total > 0")
          ("annotations"
           ("summary"
            .
            "Host EDAC Uncorrectable Errors detected (instance {{ $labels.instance }})")
           ("description"
            .
            "Host {{ $labels.instance }} has had {{ printf \"%.0f\" $value }} uncorrectable memory errors reported by EDAC in the last 5 minutes.
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostEdacUncorrectableErrorsDetected"))
         (("labels" ("severity" . "warning"))
          ("for" . "2m")
          ("expr"
           .
           "rate(node_network_receive_errs_total[2m]) / rate(node_network_receive_packets_total[2m]) > 0.01")
          ("annotations"
           ("summary"
            .
            "Host Network Receive Errors (instance {{ $labels.instance }})")
           ("description"
            .
            "Host {{ $labels.instance }} interface {{ $labels.device }} has encountered {{ printf \"%.0f\" $value }} receive errors in the last five minutes.
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostNetworkReceiveErrors"))
         (("labels" ("severity" . "warning"))
          ("for" . "2m")
          ("expr"
           .
           "rate(node_network_transmit_errs_total[2m]) / rate(node_network_transmit_packets_total[2m]) > 0.01")
          ("annotations"
           ("summary"
            .
            "Host Network Transmit Errors (instance {{ $labels.instance }})")
           ("description"
            .
            "Host {{ $labels.instance }} interface {{ $labels.device }} has encountered {{ printf \"%.0f\" $value }} transmit errors in the last five minutes.
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostNetworkTransmitErrors"))
         (("labels" ("severity" . "warning"))
          ("for" . "1m")
          ("expr"
           .
           "(rate(node_network_receive_bytes_total{device!~\"^tap.*\"}[1m]) + rate(node_network_transmit_bytes_total{device!~\"^tap.*\"}[1m])) / node_network_speed_bytes{device!~\"^tap.*\"} > 0.8 < 10000")
          ("annotations"
           ("summary"
            .
            "Host Network Interface Saturated (instance {{ $labels.instance }})")
           ("description"
            .
            "The network interface \"{{ $labels.device }}\" on \"{{ $labels.instance }}\" is getting overloaded.
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostNetworkInterfaceSaturated"))
         (("labels" ("severity" . "warning"))
          ("for" . "5m")
          ("expr"
           .
           "node_nf_conntrack_entries / node_nf_conntrack_entries_limit > 0.8")
          ("annotations"
           ("summary"
            .
            "Host conntrack limit (instance {{ $labels.instance }})")
           ("description"
            .
            "The number of conntrack is approching limit
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostConntrackLimit"))
         (("labels" ("severity" . "warning"))
          ("for" . "2m")
          ("expr"
           .
           "(node_timex_offset_seconds > 0.05 and deriv(node_timex_offset_seconds[5m]) >= 0) or (node_timex_offset_seconds < -0.05 and deriv(node_timex_offset_seconds[5m]) <= 0)")
          ("annotations"
           ("summary"
            .
            "Host clock skew (instance {{ $labels.instance }})")
           ("description"
            .
            "Clock skew detected. Clock is out of sync.
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostClockSkew"))
         (("labels" ("severity" . "warning"))
          ("for" . "2m")
          ("expr"
           .
           "min_over_time(node_timex_sync_status[1m]) == 0 and node_timex_maxerror_seconds >= 16")
          ("annotations"
           ("summary"
            .
            "Host clock not synchronising (instance {{ $labels.instance }})")
           ("description"
            .
            "Clock not synchronising.
  VALUE = {{ $value }}
  LABELS = {{ $labels }}"))
          ("alert" . "HostClockNotSynchronising"))))
      ("name" . "node-exporter")))))
