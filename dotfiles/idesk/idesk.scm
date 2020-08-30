#!/usr/bin/env -S guile --no-auto-compile
!#

(use-modules (ice-9 match))

(define x
  (make-parameter 39))

(define y
  (make-parameter 29))

(for-each (match-lambda ((file caption command icon)
                         (with-output-to-file file
                           (lambda ()
                             (display "table Icon\n")
                             (format #t "  Caption: ~a~%" caption)
                             (format #t "  Command: ~a~%" command)
                             (format #t "  Icon: ~a~%" icon)
                             (display "  Width: 48\n")
                             (display "  Height: 48\n")
                             (format #t "  X: ~a~%" (x))
                             (format #t "  Y: ~a~%" (y (+ (y) 100)))
                             (display "end\n")))))
          '(("xterm.lnk"
             "XTerm"
             "/home/oleg/.guix-profile/bin/xterm"
             "/home/oleg/.guix-profile/share/icons/gnome/48x48/apps/gnome-terminal.png")
            ("guix.vm.wugi.info.lnk"
             "VNC guix.vm"
             "vnc vm"
             "/home/oleg/.guix-profile/share/icons/hicolor/48x48/apps/tigervnc.png")))
