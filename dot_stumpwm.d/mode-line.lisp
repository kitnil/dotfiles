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
          ,'(:eval (let* ((group (current-group))
                          (frame (tile-group-current-frame group))
                          (windows (frame-sort-windows group frame))
                          (windows-names
                            (mapcar (lambda (window)
                                      (let ((wn (window-name window)))
                                        (format nil "~a:[~a]"
                                                (window-number window)
                                                (if (> (length wn) 10)
                                                    (concat (subseq wn 0 10) "...")
                                                    wn))))
                                    windows)))
                     (if (> (length windows-names) 5)
                         (format nil "(~a ...)" (join (take 5 windows-names)))
                         (format nil "(~a)" (join windows-names)))))

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

          ,@(if (and *imap-recent* (not (= *imap-recent* 0)))
                (list (make-string 4 :initial-element #\space))
                '())
          ,@(if (and *imap-recent* (not (= *imap-recent* 0)))
                (list '(:eval (format nil "INBOX: ~a" *imap-recent*)))
                '())

          ,(make-string 4 :initial-element #\space)
          ,'(:eval (kubectl-current-context *kubernetes-current-cluster* t))

          ,@(if (mountpoint-free? "/srv")
                (list (make-string 4 :initial-element #\space))
                '())
          ,@(if (mountpoint-free? "/srv")
                (list '(:eval (format nil "/srv: ~a" *disk-free-srv-counter*)))
                '())

          ,@(if (mountpoint-free? "/")
                (list (make-string 4 :initial-element #\space))
                '())
          ,@(if (mountpoint-free? "/")
                (list '(:eval (format nil "/: ~a" *disk-free-root-counter*)))
                '())

          ;; ,(make-string 4 :initial-element #\space)
          ;; ,'(:eval (fmt-temp-current (temp-current) t))

          ,(make-string 4 :initial-element #\space)
          ,'(:eval (fmt-mem-available (mem-usage) t))

          ,@(if (string-equal *mjru-tapvpn-ip* "")
                '()
                (list (make-string 4 :initial-element #\space)))

          ,@(if (string-equal *mjru-tapvpn-ip* "")
                '()
                (list '(:eval (format nil "VPN 1: ~a" *mjru-tapvpn-ip*))))

          ,(make-string 4 :initial-element #\space)
          ,'(:eval (format nil "VOL: ~a" *volume-current*))

          ,(make-string 4 :initial-element #\space)
          "%d"

          ,(make-string 4 :initial-element #\space))))

(mode-line-update)

(mapcar (lambda (func)
          (add-hook *start-hook* func))
        (list (lambda ()
                ;; Get VPN IP address and set it to *mjru-tapvpn-ip* variable.
                (sb-thread:make-thread
                 (lambda ()
                   (loop while t do
                        (progn (mjru-ip-address-vpn-update) (sleep 10))))
                 :name "mjru-ip-address-vpn-update"))

	      ;; TODO: Move to mjru.lisp
              ;; (lambda () (sb-thread:make-thread
              ;;        (lambda ()
              ;;          (loop while t do
              ;;               (progn (mjru-hms-current-stack-update) (sleep 10))))
              ;;        :name "mjru-hms-current-stack-update"))

              (lambda ()
                ;; (sb-thread:make-thread ||#
                ;;  (lambda () ||#
                ;;    (loop while t do ||#
                ;;         (progn (if (gpg-key-opened? (concat (getenv "HOME") ||#
                ;;                                               "/.password-store/localhost/imap/oleg.gpg")) ||#
                ;;                    (imap-update-recent-count) ||#
                ;;                    (setq *imap-recent* nil)) ||#
                ;;                (sleep 60)))) ||#
                ;;  :name "imap-update-recent-count") ||#

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
                          (kubernetes-update-current-cluster)
                          (sleep 60))))
                 :name "mode-line-kubernetes-current-cluster")

                (sb-thread:make-thread
                 (lambda ()
                   (kubernetes-update-current-cluster-inotify))
                 :name "mode-line-kubernetes-current-cluster-inotify")

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

(defcommand mode-line-start-programs () ()
  (flet ((all-start-programs ()
           (filter (lambda (str)
                     (uiop/utility:string-suffix-p str "-start-programs"))
                   (all-commands))))
    (run-commands (first (filter (lambda (str)
                                   (string-contains (write-to-string (group-number (current-group))) str))
                                 (all-start-programs))))))

;; See events.lisp in StumpWM source.
(setq *mode-line-click-hook*
      (list (lambda (mode-line button x y)
              (case button
                ((1) (mode-line-start-programs))
                ((2) (volume-toggle))
                ((3) (delete-window))
                ((4) (cond ((> x (- (screen-width (group-screen (current-group))) 30))
                            (volume-increase))
                           ((> x 200)
                            (prev-in-frame))
                           (t
                            (gprev))))
                ((5) (cond ((> x (- (screen-width (group-screen (current-group))) 30))
                            (volume-decrease))
                           ((> x 200)
                            (next-in-frame))
                           (t
                            (gnext))))
                ((8) (volume-decrease))
                ((9) (volume-increase))))))
