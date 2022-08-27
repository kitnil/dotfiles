(in-package :stumpwm)


;;;
;;; VPN
;;;

(defvar *bq-tun1-ip* "")

(defcommand bq-ip-address-vpn-update () ()
  (setq *bq-tun1-ip* (network-address "tun1")))
