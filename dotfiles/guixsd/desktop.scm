(use-modules (gnu))

(use-service-modules desktop dbus networking)
(use-package-modules avahi suckless)

(list (udisks-service)
      ;; (service upower-service-type)
      (service accountsservice-service-type)
      (service colord-service-type)
      (geoclue-service)
      (service polkit-service-type)
      (dbus-service #:services (list avahi))
      (elogind-service)
      (service ntp-service-type))
