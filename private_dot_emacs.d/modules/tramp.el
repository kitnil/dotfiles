;; Make sure we work on remote guixsd machines :)
;; probably only helps if you start on a guixsd machine..!
(with-eval-after-load 'tramp
  (setq tramp-remote-path
	(append tramp-remote-path
		'(tramp-own-remote-path))))
