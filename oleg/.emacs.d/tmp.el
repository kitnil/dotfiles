(defun supeng-find-routers ()
  (interactive)
  (mapcar 'find-file
          '("/ssh:root@78.108.91.85:/root/deploy"
            "/ssh:root@78.108.93.115:/root/deploy"
            "/ssh:root@78.108.90.34:/root/deploy"
            "/ssh:root@78.108.89.188:/root/deploy")))
