'(("groups" .
   #((("rules"
       .
       #((("labels" ("severity" . "critical"))
          ("for" . "0m")
          ("expr" . "probe_success == 0")
          ("annotations"
           ("summary"
            .
            "Blackbox probe failed (instance {{ $labels.instance }})")
           ("description"
            .
            "Probe failed\n  VALUE = {{ $value }}\n  LABELS = {{ $labels }}"))
          ("alert" . "BlackboxProbeFailed"))
         (("labels" ("severity" . "warning"))
          ("for" . "1m")
          ("expr"
           .
           "avg_over_time(probe_duration_seconds[1m]) > 10")
          ("annotations"
           ("summary"
            .
            "Blackbox slow probe (instance {{ $labels.instance }})")
           ("description"
            .
            "Blackbox probe took more than 1s to complete\n  VALUE = {{ $value }}\n  LABELS = {{ $labels }}"))
          ("alert" . "BlackboxSlowProbe"))
         (("labels" ("severity" . "critical"))
          ("for" . "0m")
          ("expr"
           .
           "probe_http_status_code <= 199 OR probe_http_status_code >= 400")
          ("annotations"
           ("summary"
            .
            "Blackbox probe HTTP failure (instance {{ $labels.instance }})")
           ("description"
            .
            "HTTP status code is not 200-399\n  VALUE = {{ $value }}\n  LABELS = {{ $labels }}"))
          ("alert" . "BlackboxProbeHttpFailure"))
         (("labels" ("severity" . "warning"))
          ("for" . "0m")
          ("expr"
           .
           "probe_ssl_earliest_cert_expiry - time() < 86400 * 30")
          ("annotations"
           ("summary"
            .
            "Blackbox SSL certificate will expire soon (instance {{ $labels.instance }})")
           ("description"
            .
            "SSL certificate expires in 30 days\n  VALUE = {{ $value }}\n  LABELS = {{ $labels }}"))
          ("alert"
           .
           "BlackboxSslCertificateWillExpireSoon"))
         (("labels" ("severity" . "critical"))
          ("for" . "0m")
          ("expr"
           .
           "probe_ssl_earliest_cert_expiry - time() < 86400 * 3")
          ("annotations"
           ("summary"
            .
            "Blackbox SSL certificate will expire soon (instance {{ $labels.instance }})")
           ("description"
            .
            "SSL certificate expires in 3 days\n  VALUE = {{ $value }}\n  LABELS = {{ $labels }}"))
          ("alert"
           .
           "BlackboxSslCertificateWillExpireSoon"))
         (("labels" ("severity" . "critical"))
          ("for" . "0m")
          ("expr"
           .
           "probe_ssl_earliest_cert_expiry - time() <= 0")
          ("annotations"
           ("summary"
            .
            "Blackbox SSL certificate expired (instance {{ $labels.instance }})")
           ("description"
            .
            "SSL certificate has expired already\n  VALUE = {{ $value }}\n  LABELS = {{ $labels }}"))
          ("alert" . "BlackboxSslCertificateExpired"))
         (("labels" ("severity" . "warning"))
          ("for" . "1m")
          ("expr"
           .
           "avg_over_time(probe_http_duration_seconds[1m]) > 1")
          ("annotations"
           ("summary"
            .
            "Blackbox probe slow HTTP (instance {{ $labels.instance }})")
           ("description"
            .
            "HTTP request took more than 1s\n  VALUE = {{ $value }}\n  LABELS = {{ $labels }}"))
          ("alert" . "BlackboxProbeSlowHttp"))
         (("labels" ("severity" . "warning"))
          ("for" . "1m")
          ("expr"
           .
           "avg_over_time(probe_icmp_duration_seconds[1m]) > 1")
          ("annotations"
           ("summary"
            .
            "Blackbox probe slow ping (instance {{ $labels.instance }})")
           ("description"
            .
            "Blackbox ping took more than 1s\n  VALUE = {{ $value }}\n  LABELS = {{ $labels }}"))
          ("alert" . "BlackboxProbeSlowPing"))))
      ("name" . "blackbox-exporter")))))
