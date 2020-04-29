(defvar ange-ftp-hosts-no-pasv '("localhost")
  "*List of hosts that do not need PASV (e.g. hosts within your firewall).
  Used by `ange-ftp-set-passive'.")	; rephrased, added "*" // era

(defun ange-ftp-set-passive ()
  "Function to send a PASV command to hosts not named in the variable
  `ange-ft-hosts-no-pasv'. Intended to be called from the hook variable
  `ange-ftp-process-startup-hook'."	; rephrased significantly // era
  (if (not (member host ange-ftp-hosts-no-pasv))
      (ange-ftp-raw-send-cmd proc "passive")))

(add-hook 'ange-ftp-process-startup-hook 'ange-ftp-set-passive)
