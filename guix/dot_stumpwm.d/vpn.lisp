(in-package :stumpwm)

(defun network-address (interface)
  (string-trim '(#\Newline)
               (run-shell-command
                (join (list "ip --json address"
                            (format nil "jq --raw-output '.[] | select(.ifname == ~s) | .addr_info[] | select(.\"family\" == \"inet\") | .local'"
                                    interface))
                      #\|)
                t)))
