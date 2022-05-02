(require :swank)

(swank-loader:init)

(defcommand swank (port) ((:string "Port number: "))
  (sb-thread:make-thread
   (lambda ()
     (let ((swank::*loopback-interface* "127.0.0.1"))
       (swank:create-server :port (parse-integer port)
                            :dont-close t)))
   :name "swank"))
