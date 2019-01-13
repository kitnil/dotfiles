(define-module (sysadmin sensors)
  #:use-module (guix gexp)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages linux)
  #:export (zabbix-sensors))

(define zabbix-sensors
  (let* ((sensors (file-append lm-sensors "/bin/sensors"))
         (tail    (file-append coreutils "/bin/tail"))
         (head    (file-append coreutils "/bin/head"))
         (gawk    (file-append gawk "/bin/gawk"))
         (cut     (file-append coreutils "/bin/cut"))
         (command (list sensors
                        "|" tail "-n" "5"
                        "|" head "-n" "4"
                        "|" gawk "-F'[:+°]'" "'{avg+=$3}END{print" "avg/NR}'"
                        "|" cut "-d" "'.'" "-f" "'1'")))
    (program-file "zabbix_sensors" #~(system (string-join (list #$@command))))))
