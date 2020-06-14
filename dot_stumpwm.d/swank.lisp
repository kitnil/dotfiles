;; https://lists.gnu.org/archive/html/help-guix/2017-01/msg00033.html

(load "/run/current-system/profile/share/common-lisp/source/sbcl-slime-swank/swank.asd")

(require :swank)

(defcommand swank (port) ((:string "Port number: "))
  (sb-thread:make-thread
   (lambda ()
     (swank:create-server :port (parse-integer port) :dont-close t))
   :name "swank"))
(mapcar (lambda (func)
          (add-hook *start-hook* func))
        (list (lambda ()
                (swank "4005"))))
