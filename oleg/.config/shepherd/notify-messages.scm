(use-modules (shepherd service))

(define notify-messages-service
  (make <service>
    #:requires '(dunst)
    #:docstring '("Send /var/log/messages to notify-send")
    #:provides '(notify-messages)
    #:start (make-forkexec-constructor
             (list "/home/oleg/src/dotfiles/oleg/bin/messages2notify-send"))
    #:stop (make-kill-destructor)
    #:respawn? #t))

(register-services notify-messages-service)
