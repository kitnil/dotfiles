(in-package :stumpwm)

(defvar *kvm-disk-free-root-counter* 0)

(defcommand kvm-disk-free-root-update-counter () ()
  (setq *kvm-disk-free-root-counter* (disk-free "/kvm" :remote "kvm25.intr")))
