(require :swank)

(swank-loader:init)

(defcommand swank (port) ((:string "Port number: "))
  (sb-thread:make-thread
   (lambda ()
     (swank:create-server :port (parse-integer port) :dont-close t))
   :name "swank"))
