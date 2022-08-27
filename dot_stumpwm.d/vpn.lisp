(in-package :stumpwm)

(defvar *tapvpn-ip* "")

(defcommand ip-address-vpn-update () ()
  (setq *tapvpn-ip*
        (string-trim '(#\Newline)
                     (run-shell-command
                      (join '("ip --json address"
                              "jq --raw-output '.[] | select(.ifname == \"tapvpn\") | .addr_info[] | select(.\"family\" == \"inet\") | .local'")
                            #\|)
                      t))))
