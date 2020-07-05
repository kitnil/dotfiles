(in-package :stumpwm)

(require :stumptray)

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

	  ;; TODO: Move to mjru.lisp
          ;; ,@(if (or (equal *mjru-hms-current-stack* "")
          ;;           (equal *covid-19-count* ""))
          ;;       '()
          ;;       (list (make-string 4 :initial-element #\space)))

	  ;; TODO: password-store-show
          ;; ,@(if (free-time?)
          ;;       (if (equal *covid-19-count* "")
          ;;           '()
          ;;           (list '(:eval (format nil "COVID-19: ~a"
          ;;                          (let ((count (split-string *covid-19-count* '(#\:))))
          ;;                            (join (list (first count)
          ;;                                        (format nil "^[^B^2*~a^]" (third count))
          ;;                                        (format nil "^[^B^1*~a^]" (second count)))
          ;;                                  #\:))))))
          ;;       (if (equal *mjru-hms-current-stack* "")
          ;;           '()
          ;;           (list '(:eval (format nil "stack: ~a" *mjru-hms-current-stack*)))))

          ,@(if (= *torrent-seeds-counter* 0)
                '()
                (list (make-string 4 :initial-element #\space)))
          ,@(if (= *torrent-seeds-counter* 0)
                '()
                (list '(:eval (format nil "TOR_SEED: ~a" *torrent-seeds-counter*))))

          ,@(if (and *imap-recent* (not (= *imap-recent* 0)))
                (list (make-string 4 :initial-element #\space))
                '())
          ,@(if (and *imap-recent* (not (= *imap-recent* 0)))
                (list '(:eval (format nil "INBOX: ~a" *imap-recent*)))
                '())

          ,(make-string 4 :initial-element #\space)
          ,'(:eval (format nil "/srv: ~a" *disk-free-srv-counter*))

          ,(make-string 4 :initial-element #\space)
          ,'(:eval (format nil "/: ~a" *disk-free-root-counter*))

          ,@(if *spb-disk-free-root-counter*
                (list (make-string 4 :initial-element #\space))
                '())
          ,@(if *spb-disk-free-root-counter*
                (list '(:eval (format nil "spb: /: ~a" *spb-disk-free-root-counter*)))
                '())

          ,(make-string 4 :initial-element #\space)
          ,'(:eval (fmt-temp-current (temp-current) t))

          ,(make-string 4 :initial-element #\space)
          ,'(:eval (fmt-mem-available (mem-usage) t))

          ,@(if (string-equal *tapvpn-ip* "")
                '()
                (list (make-string 4 :initial-element #\space)))

          ,@(if (string-equal *tapvpn-ip* "")
                '()
                (list '(:eval (format nil "VPN: ~a" *tapvpn-ip*))))

          ,(make-string 4 :initial-element #\space)
          ,'(:eval (format nil "VOL: ~a" *volume-current*))

          ,(make-string 4 :initial-element #\space)
          "%d"

          ,(make-string 4 :initial-element #\space))))

(defvar *tapvpn-ip* "")
(defcommand ip-address-vpn-update () ()
  (setq *tapvpn-ip*
        (string-trim '(#\Newline)
                     (run-shell-command
                      (join '("ip --json address"
                              "jq --raw-output '.[] | select(.ifname == \"tap0\") | .addr_info[] | select(.\"family\" == \"inet\") | .local'")
                            #\|)
                      t))))

(mode-line-update)
(mode-line)
(stumptray:stumptray)

(mapcar (lambda (func)
          (add-hook *start-hook* func))
        (list (lambda ()
                ;; Get VPN IP address and set it to *tapvpn-ip* variable.
                (sb-thread:make-thread
                 (lambda ()
                   (loop while t do
                        (progn (ip-address-vpn-update) (sleep 10))))
                 :name "ip-address-vpn-update"))

	      ;; TODO: Move to mjru.lisp
              ;; (lambda () (sb-thread:make-thread
              ;;        (lambda ()
              ;;          (loop while t do
              ;;               (progn (mjru-hms-current-stack-update) (sleep 10))))
              ;;        :name "mjru-hms-current-stack-update"))

              (lambda ()
                (sb-thread:make-thread
                 (lambda ()
                   (loop while t do
                        (progn (if (gpg-key-opened? (concat (getenv "HOME")
                                                              "/.password-store/localhost/imap/oleg.gpg"))
                                   (imap-update-recent-count)
                                   (setq *imap-recent* nil))
                               (sleep 60))))
                 :name "imap-update-recent-count")

                (sb-thread:make-thread
                 (lambda ()
                   (loop while t do
                        (progn
                          (disk-free-srv-update-counter)
                          (sleep 60))))
                 :name "disk-free-srv-update-counter")

                (sb-thread:make-thread
                 (lambda ()
                   (loop while t do
                        (progn
                          (disk-free-root-update-counter)
                          (sleep 60))))
                 :name "disk-free-root-update-counter")

                (sb-thread:make-thread
                 (lambda ()
                   (loop while t do
                        (progn
                          (spb-disk-free-root-update-counter)
                          (sleep 60))))
                 :name "mode-line-df-spb")

                (sb-thread:make-thread
                 (lambda ()
                   (loop while t do
                        (progn
                          (torrent-seeds-update-counter)
                          (mode-line-update)
                          (sleep 60))))
                 :name "torrent-seeds-update-counter")

                ;; (sb-thread:make-thread
                ;;  (lambda ()
                ;;    (loop while t do
                ;;         (progn
                ;;           (run-shell-command "notmuch new")
                ;;           (mode-line-update)
                ;;           (sleep (* 60 60)))))
                ;;  :name "notmuch")

                (sb-thread:make-thread
                 (lambda ()
                   (loop while t do
                        (progn
                          (covid-19-update-count)
                          (sleep (* (* 60 60) 6)))))
                 :name "covid-19"))))
