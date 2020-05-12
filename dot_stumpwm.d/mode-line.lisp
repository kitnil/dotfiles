(in-package :stumpwm)

(setf *mode-line-position* :bottom)

(defvar *mode-line-window-number* 10)

(setf *group-format* "%t")

(setf *mode-line-timeout* 2)
(setf *TIME-MODELINE-STRING* "%a, %e %b %Y %k:%M")

(setf *mode-line-pad-x* 10)
(setf *mode-line-pad-y* 5)
(setf *mode-line-border-width* 0)

(defcommand mode-line-update () ()
  (setf *screen-mode-line-format*
        `("%g"
          ,(make-string 4 :initial-element #\space)
          ,'(:eval (let* ((window (current-window))
                          (wn (window-name window)))
                     (format nil "~a:[~a]"
                             (window-number window)
                             (if (> (length wn) 10)
                                 (concat (subseq wn 0 10) "...")
                                 wn))))
          ,(make-string 4 :initial-element #\space)
          "^>"
          ,@(if (equal *covid-19-count* "")
                '()
                (list (make-string 4 :initial-element #\space)))
          ,@(if (equal *covid-19-count* "")
                '()
                (list '(:eval (format nil "COVID-19: ~a"
                               (let ((count (split-string *covid-19-count* '(#\:))))
                                 (join (list (first count)
                                             (format nil "^[^B^2*~a^]" (third count))
                                             (format nil "^[^B^1*~a^]" (second count)))
                                       #\:))))))
          ,@(if (= *torrent-seeds-counter* 0)
                '()
                (list (make-string 4 :initial-element #\space)))
          ,@(if (= *torrent-seeds-counter* 0)
                '()
                (list '(:eval (format nil "TOR_SEED: ~a" *torrent-seeds-counter*))))
          ,@(if (= *imap-recent* 0)
                '()
                (list (make-string 4 :initial-element #\space)))
          ,@(if (= *imap-recent* 0)
                '()
                (list '(:eval (format nil "INBOX: ~a" *imap-recent*))))
          ,(make-string 4 :initial-element #\space)
          ,'(:eval (format nil "/: ~a" *disk-free-root-counter*))
          ,(make-string 4 :initial-element #\space)
          ,'(:eval (format nil "spb: /: ~a" *spb-disk-free-root-counter*))
          ,(make-string 4 :initial-element #\space)
          ,'(:eval (format nil "TEMP: ~a" (temp-current)))
          ,(make-string 4 :initial-element #\space)
          ,'(:eval (format nil "MEM: ~a" (fmt-mem-available (mem-usage))))
          ,(make-string 4 :initial-element #\space)
          ,'(:eval (format nil "VPN: ~a" *tapvpn-ip*))
          ,(make-string 4 :initial-element #\space)
          ,'(:eval (format nil "VOL: ~a" *volume-current*))
          ,(make-string 4 :initial-element #\space)
          "%d")))

(mode-line-update)
(mode-line)

(defcommand ip-address-vpn-update () ()
  (setq *tapvpn-ip*
        (string-trim '(#\Newline)
                     (run-shell-command
                      (join '("ip --json address"
                              "jq --raw-output '.[] | select(.ifname == \"tapvpn\") | .addr_info[] | select(.\"family\" == \"inet\") | .local'")
                            #\|)
                      t))))

(mapcar (lambda (func)
          (add-hook *start-hook* func))
        (list (lambda ()
                ;; Get VPN IP address and set it to *tapvpn-ip* variable.
                (sb-thread:make-thread
                 (lambda ()
                   (loop while t do
                        (progn (ip-address-vpn-update) (sleep 10))))))
              (lambda ()
                (sb-thread:make-thread
                 (lambda ()
                   (loop while t do
                        (progn (imap-update-recent-count) (sleep 60)))))
                (sb-thread:make-thread
                 (lambda ()
                   (loop while t do
                        (progn
                          (disk-free-root-update-counter)
                          (sleep 60)))))
                (sb-thread:make-thread
                 (lambda ()
                   (loop while t do
                        (progn
                          (torrent-seeds-update-counter)
                          (mode-line-update)
                          (sleep 60)))))
                (sb-thread:make-thread
                 (lambda ()
                   (loop while t do
                        (progn
                          (run-shell-command "notmuch new")
                          (mode-line-update)
                          (sleep (* 60 60))))))
                (sb-thread:make-thread
                 (lambda ()
                   (loop while t do
                        (progn
                          (covid-19-update-count)
                          (sleep (* (* 60 60) 6)))))))))
