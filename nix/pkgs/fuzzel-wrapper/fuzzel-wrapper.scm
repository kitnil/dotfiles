#!/run/current-system/sw/bin/guile \
--no-auto-compile -e main -s
!#
(use-modules (ice-9 match))

(define main
  (match-lambda ((command args ...)
                 (match args
                   ((head tail ...)
                    (and=> (getenv "FUZZEL_DESKTOP_FILE_ID")
                           (match-lambda
                             ("com.obsproject.Studio.desktop"
                              ;; Capture Wayland desktop.
                              (and=> (getenv "GUIX_DBUS_SESSION_BUS_ADDRESS")
                                     (lambda (guix-dbus-session-bus-address)
                                       (setenv "DBUS_SESSION_BUS_ADDRESS"
                                               guix-dbus-session-bus-address))))
                             (_ #f)))
                    (apply execl
                           (append (list "/usr/bin/env" "/usr/bin/env" head)
                                   tail)))))))
