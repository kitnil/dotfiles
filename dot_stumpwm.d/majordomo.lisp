(in-package :stumpwm)


;;;
;;; Variables
;;;

(defvar majordomo-webs
  ;; without web19
  (mapcar (lambda (x)
            (sb-unicode:lowercase (string x)))
          '(web15 web16 web17 web18
            web20 web21 web22 web23 web25 web26 web27 web28 web29
            web30 web31 web32 web33 web34 web35 web36 web37)))

(defvar majordomo-dh
  (mapcar (lambda (x)
            (sb-unicode:lowercase (string x)))
          '(dh1-mr dh2-mr dh3-mr)))

(defvar majordomo-vpn
  (mapcar (lambda (x)
            (concat "vpn-" (sb-unicode:lowercase (string x)) ".majordomo.ru"))
          '(miran dh office)))


;;;
;;; HMS
;;;

(defvar *majordomo-hms-current-stack* "")

(defcommand majordomo-hms-current-stack () ()
  (run-shell-command
   (format nil
           "~a | jq --join-output .active"
           (join
            (list "curl"
                  "--user" (format nil "jenkins:~a"
                                   (password-store-show "majordomo/jenkins/jenkins"))
                  "--request" "GET" "http://nginx1.intr:8080/hms")))
   t))

(defcommand majordomo-hms-current-stack-update () ()
  (setq *majordomo-hms-current-stack* (majordomo-hms-current-stack)))

(defcommand majordomo-mongo-production () ()
  (term-shell-command (concat "mongo mongodb://admin:"
                              (password-store-show "majordomo/mongo/ci.intr/admin")
                              "@hms01-mr.intr:27017,hms02-mr.intr:27017,hms03-mr.intr:27017/admin?replicaSet=hms-rs0")
                      :scrollbar t
                      :title "mongo-prod"))

(defcommand majordomo-mongo-development () ()
  (term-shell-command (concat "mongo mongodb://admin:"
                              (password-store-show "majordomo/mongo/ci.intr/admin")
                              "@ci.intr:27017/admin")
                      :scrollbar t
                      :title "mongo-dev"))


;;;
;;; Monitoring
;;;

(defcommand majordomo-alerta-top () ()
  (term-shell-command (join (list "sh" "-c" (format nil "~s" "while true; do /home/oleg/.local/bin/alerta top; sleep 20; done")))
                      :title "alerta-top"
                      :terminal 'st
                      :font "Monospace:size=6"))

(defcommand majordomo-vnc-grafana () ()
  (run-shell-command "vncviewer 172.16.100.182:5900"))

(defcommand majordomo-alerta () ()
    (firefox "https://alerta.intr" "Alerta"))

(defcommand majordomo-grafana () ()
  (firefox "https://grafana.intr/" "Grafana"))

(defcommand majordomo-grafana-netflow () ()
  (firefox "https://grafana.intr/d/000000042/netflow?orgId=1&refresh=1m" "Netflow"))

(defcommand majordomo-grafana-upstream-interfaces () ()
  (firefox "https://grafana.intr/d/6QgXJjmik/upstream-interfaces-traffic?orgId=1" "Upstream interfaces"))

(defcommand majordomo-check-website () ()
  (firefox "https://www.uptrends.com/tools/uptime"))

(defcommand majordomo-zabbix () ()
  (firefox "https://zabbix.intr/dashboard.php?fullscreen=1" "Dashboard"))

(defcommand majordomo-kibana () ()
  (firefox "https://kibana.intr/" "Kibana"))

(defcommand majordomo-kibana-alerta () ()
  (run-shell-command "chromium --app=https://kibana.intr/goto/d63fb3a2e0b36deacc8f73f53cc14b4d"))


;;;
;;; VNC
;;;

(defcommand majordomo-vnc () ()
  "Connect to Majordomo VNC"
  (run-shell-command "majordomo-vnc"))

(define-key *root-map* (kbd "V") "majordomo-vnc")


;;;
;;; Switches
;;;

(defun cisco-connect-command (host)
  (join (list "env" (format nil "TELNET_PASSWORD=~s" (password-store-show "majordomo/general"))
              "cisco-interact" host)))

(defun cisco-connect (host)
  (term-shell-command (cisco-connect-command host)
                      :title (format nil "telnet-~a" host)
                      :scrollbar t))

(defmacro define-mj-cisco (command)
  `(progn
     (defcommand ,command () ()
       (cisco-connect ,(sb-unicode:lowercase (string command))))))

(define-mj-cisco sw1-mr11.intr)
(define-mj-cisco sw1-mr12.intr)
(define-mj-cisco sw2-mr12.intr)
(define-mj-cisco sw1-mr14.intr)
(define-mj-cisco sw1-mr116.intr)
(define-mj-cisco sw1-mr143.intr)
(define-mj-cisco sw1-dh507.intr)


;;;
;;; Hardware
;;;

(defcommand majordomo-ipmi (host) ((:string "Hostname: "))
  (run-shell-command (join (list (concat (getenv "HOME")
                                         "/.nix-profile/bin/ipmi")
                                 host))))

(defcommand majordomo-ipmiview () ()
  (run-shell-command (concat (getenv "HOME")
                             "/.nix-profile.d/ipmiview/ipmiview/bin/IPMIView")))

(defcommand majordomo-ipkvm () ()
  (run-shell-command
   (join (list "firefox-esr-52" "-P" "esr52" "--new-instance"))))


;;;
;;; WEB
;;;

(defcommand majordomo-run-firefox () ()
  (gselect "1")
  (firefox)
  (renumber 2)
  (sleep 5)
  (sb-thread:make-thread
   (lambda ()
     (sleep 5)

     (gselect "3")
     (browse-url-firefox "https://grafana.intr/d/ogvzsY3mb/web-performance-panelized" t t)
     (browse-url-firefox "https://mjru.slack.com/" t t)
     (sleep 10)

     (gselect "5")
     (browse-url-firefox "https://jenkins.wugi.info/view/Failed/" t t)
     (sleep 5)

     (gselect "7")
     (browse-url-firefox "https://billing2.intr/servers?sort_by=name&sort_order=1&equip_server_type_id=3" t)
     (sleep 5)

     (gselect "8")
     (browse-url-firefox "https://alerta.intr" t)
     (browse-url-firefox "https://kibana.intr/goto/d63fb3a2e0b36deacc8f73f53cc14b4d" t t)
     (sleep 10)

     (gselect "1"))
   :name "majordomo-run-firefox"))


;;;
;;; Tickets
;;;

(defcommand majordomo-cerb () ()
  (run-shell-command
   (join (list (concat "CERBERUS_KEY="
                       (password-store-show "cerberus.intr/api/notification/key"))
               (concat "CERBERUS_SECRET="
                       (password-store-show "cerberus.intr/api/notification/secret"))
               "cerb"))))


;;;
;;; SSH
;;;

(defcommand majordomo-xpanes-vpn-ssh () ()
  (term-shell-command (join `("xpanes -t -C 1 -c 'ssh {}'" ,@majordomo-vpn))
                      :title "xpanes-vpn-ssh"
                      :font '("-fa" "Monospace" "-fs" "6")))

(defcommand majordomo-xpanes-dh-ssh () ()
  (term-shell-command (join `("xpanes -t -c 'ssh {}.intr'" ,@majordomo-dh))
                      :title "xpanes-dh-ssh"
                      :font '("-fa" "Monospace" "-fs" "6")))

(defcommand majordomo-xpanes-web-ssh () ()
  (term-shell-command (join `("xpanes -t -c 'ssh {}.intr'" ,@majordomo-webs))
                      :title "xpanes-web-ssh"
                      :font '("-fa" "Monospace" "-fs" "6")))

(defcommand majordomo-xpanes-web-top () ()
  (term-shell-command (join `("xpanes -c 'ssh -t {}.intr top'" ,@majordomo-webs))
                      :title "xpanes-web-top"
                      :font '("-fa" "Monospace" "-fs" "6")))

(defcommand majordomo-xpanes-ssh-nginx () ()
  (term-shell-command (join `(,(xpanes-command "ssh -t {}")
                               ,@'("nginx1-mr.intr" "nginx2-mr.intr")))
                      :title "xpanes-routers"))

(defcommand majordomo-xpanes-ssh-ns () ()
  (term-shell-command (join `(,(xpanes-command "ssh -t {}")
                               ,@'("ns1-mr.intr" "ns2-mr.intr" "ns1-dh.intr" "ns2-dh.intr")))
                      :title "xpanes-routers"))

(defcommand majordomo-xpanes-routers () ()
  (term-shell-command (join `(,(xpanes-command "ssh -t {}")
                               ,@'("router4.intr" "vpn-miran.majordomo.ru" "vpn-dh.majordomo.ru")))
                      :title "xpanes-routers"))


;;;
;;; CI
;;;

(defvar *majordomo-jenkins-url*
  "https://jenkins.intr")

(defcommand majordomo-jenkins-webservices () ()
  (browse-url-firefox (format nil "~a/job/webservices" *majordomo-jenkins-url*) t))

(define-key *top-map* (kbd "M-s-W") "majordomo-jenkins-webservices")

(defcommand majordomo-jenkins-group (group) ((:string "Group: "))
  (browse-url-firefox (format nil "~a/job/~a" *majordomo-jenkins-url* group) t))


;;;
;;; Emacs
;;;

(defcommand majordomo-mj-installed-servers () ()
  (progn (run-shell-command "emacsclient --eval '(mj-installed-servers)'")
         (switch-to-emacs)))


;;;
;;; MySQL
;;;

(defcommand majordomo-xpanes-web-mycli () ()
  (term-shell-command (join `(,(xpanes-command
                                (format nil "mycli --password ~a -d {}"
                                        (password-store-show "majordomo/web/mysql/root")))
                               ,@majordomo-webs))
                      :title "xpanes-web-mycli"
                      :font '("-fa" "Monospace" "-fs" "6")))


;;;
;;; Autotype
;;;

(defcommand majordomo-pass-eng () ()
  (if (y-or-n-p "Insert eng password and press Enter? ")
      (window-send-string
       (format nil "~a~%" (password-store-show "majordomo/ssh/eng")))))

(defcommand majordomo-pass-sup () ()
  (if (y-or-n-p "Insert sup password and press Enter? ")
      (window-send-string
       (format nil "~a~%" (password-store-show "majordomo/ssh/sup")))))

(defcommand majordomo-pass-route () ()
  (if (y-or-n-p "Insert router password and press Enter? ")
      (window-send-string
       (format nil "~a~%" (password-store-show "majordomo/ssh/router")))))

(defcommand majordomo-pass-ipmi () ()
  (if (y-or-n-p "Insert IPMI ADMIN password and press Enter? ")
      (window-send-string
       (format nil "~a" (password-store-show "majordomo/ipmi/ADMIN")))))


;;;
;;; Docker
;;;

(defcommand majordomo-docker-pull () ()
  (window-send-string (format nil "~a~%" "docker ps --format '{{ .Image }}' | grep master | sort -u | xargs -I{} docker pull {}")))


;;;
;;; Office
;;;

(defcommand majordomo-office-shedule-eng () ()
  (run-shell-command "firefox --new-window https://office.majordomo.ru/shedule2/10"))

(defcommand majordomo-office-shedule-sup () ()
  (run-shell-command "firefox --new-window https://office.majordomo.ru/shedule2/2"))
