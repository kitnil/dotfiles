(in-package :stumpwm)

(require :swm-gaps)

;; Head gaps run along the 4 borders of the monitor(s)
(setf swm-gaps:*head-gaps-size* 5)

;; Inner gaps run along all the 4 borders of a window
(setf swm-gaps:*inner-gaps-size* 5)

(setf swm-gaps:*outer-gaps-size* 0)
