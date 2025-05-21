(in-package :stumpwm)


;;;
;;; Variables
;;;

(defvar mjru-webs
  ;; without web19
  (mapcar (lambda (x)
            (sb-unicode:lowercase (string x)))
          '(web15 web16 web17 web18
            web20 web21 web22 web23 web25 web26 web27 web28 web29
            web30 web31 web32 web33 web34 web35 web36 web37)))

(defvar mjru-dh
  (mapcar (lambda (x)
            (sb-unicode:lowercase (string x)))
          '(dh1-mr dh2-mr dh3-mr dh4-mr)))

(defvar mjru-vpn
  (mapcar (lambda (x)
            (concat "vpn-" (sb-unicode:lowercase (string x)) ".majordomo.ru"))
          '(miran dh office)))


;;;
;;; HMS
;;;

(defvar *mjru-hms-current-stack* "")

(defcommand mjru-hms-current-stack () ()
  (run-shell-command
   (format nil
           "~a | jq --join-output .active"
           (join
            (list "curl"
                  "--user" (format nil "jenkins:~a"
                                   (password-store-show "majordomo/private/jenkins/jenkins"))
                  "--request" "GET" "http://nginx1.intr:8080/hms")))
   t))

(defcommand mjru-hms-current-stack-update () ()
  (setq *mjru-hms-current-stack* (mjru-hms-current-stack)))

(defcommand mjru-mongo-production () ()
  (term-shell-command (concat "mongo mongodb://admin:"
                              (password-store-show "majordomo/public/mongo/ci.intr/admin")
                              "@hms01-mr.intr:27017,hms02-mr.intr:27017,hms03-mr.intr:27017/admin?replicaSet=hms-rs0")
                      :scrollbar t
                      :title "mongo-prod"))

(defun mjru-mongo-development-command ()
  (concat "mongo mongodb://admin:"
          (password-store-show "majordomo/public/mongo/ci.intr/admin")
          "@ci.intr:27017/admin"))

(defcommand mjru-mongo-development () ()
  (term-shell-command (mjru-mongo-development-command)
                      :scrollbar t :title "mongo-dev"))

(defcommand mjru-mongo-development-id-object () ()
  (let ((clipboard (get-x-selection)))
    (sb-thread:make-thread
     (lambda ()
       (message
        (run-shell-command (join (list (mjru-mongo-development-command)
                                       (format nil "--eval ~s"
                                               (format nil "load('~a/.mongorc.js'); search('~a')"
                                                       (getenv "HOME") clipboard))))
                           t))))))


;;;
;;; Monitoring
;;;

(defcommand mjru-alerta-top () ()
  (term-shell-command (join (list "sh" "-c" (format nil "~s" "while true; do alerta top; sleep 5; done")))
                      :title "alerta-top"
                      :terminal 'st
                      :font "Monospace:size=14"))

(defcommand mjru-vnc-grafana () ()
  (run-shell-command "vncviewer 172.16.100.182:5900"))

(defcommand mjru-alerta () ()
  (browse-url-firefox* "https://alerta.intr"
                       :title "Alerta"))

(defcommand mjru-grafana () ()
  (browse-url-firefox* "https://grafana.intr/"
                       :title "Grafana"))

(defcommand mjru-grafana-netflow () ()
  (browse-url-firefox* "https://grafana.intr/d/000000042/netflow?orgId=1&refresh=1m"
                       :title "Netflow"))

(defcommand mjru-grafana-upstream-interfaces () ()
  (browse-url-firefox* "https://grafana.intr/d/6QgXJjmik/upstream-interfaces-traffic?orgId=1"
                       :title "Upstream interfaces"))

(defcommand mjru-check-website () ()
  (browse-url-firefox* "https://www.uptrends.com/tools/uptime"))

(defcommand mjru-zabbix () ()
  (browse-url-firefox* "https://zabbix.intr/dashboard.php?fullscreen=1"
                       :title "Dashboard"))

(defcommand mjru-kibana () ()
  (browse-url-firefox* "https://kibana.intr/"
                       :title "Kibana"))

(defcommand mjru-kibana-alerta () ()
  (run-shell-command "chromium --app=https://kibana.intr/goto/d63fb3a2e0b36deacc8f73f53cc14b4d"))

(defcommand mjru-nexus () ()
  (browse-url-firefox* "http://nexus.intr"
                       :title "Nexus"))

(defcommand mjru-slack () ()
  (browse-url-firefox* "https://mjru.slack.com/"
                       :title "Slack"))

(defcommand mjru-cerb () ()
  (browse-url-firefox* "http://cerberus.intr/"
                       :title "Cerberus"))

(defcommand mjru-gitlab () ()
  (browse-url-firefox* "https://gitlab.intr/"
                       :title "GitLab"))

(defcommand mjru-jenkins () ()
  (browse-url-firefox* "https://jenkins.intr/"
                       :title "Jenkins"))


;;;
;;; VNC
;;;

(defcommand mjru-vnc () ()
  "Connect to Majordomo VNC"
  (run-shell-command "mjru-vnc"))

(define-key *root-map* (kbd "V") "mjru-vnc")


;;;
;;; Switches
;;;

(defun cisco-connect-command (host)
  (join (list "env" (format nil "TELNET_PASSWORD=~s" (password-store-show "majordomo/private/general"))
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

(defcommand mjru-ipmi (host) ((:string "Hostname: "))
  (run-shell-command (join (list (concat (getenv "HOME")
                                         "/.nix-profile/bin/ipmi")
                                 host))))

(defcommand mjru-ipmiview () ()
  (run-shell-command (concat (getenv "HOME")
                             "/.nix-profile.d/ipmiview/ipmiview/bin/IPMIView")))

(defcommand mjru-ipkvm () ()
  (run-shell-command
   (join (list "firefox-esr-52" "-P" "esr52" "--new-instance"))))


;;;
;;; WEB
;;;

(defcommand mjru-run-firefox () ()
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
   :name "mjru-run-firefox"))


;;;
;;; Tickets
;;;

(defcommand mjru-cerb () ()
  (run-shell-command
   (join (list (concat "CERBERUS_KEY="
                       (password-store-show "majordomo/private/cerberus.intr/api/notification/key"))
               (concat "CERBERUS_SECRET="
                       (password-store-show "majordomo/private/cerberus.intr/api/notification/secret"))
               "cerb"))))


;;;
;;; SSH
;;;

(defcommand mjru-xpanes-vpn-ssh () ()
  (term-shell-command (join `("xpanes -t -C 1 -c 'ssh {}'" ,@mjru-vpn))
                      :title "xpanes-vpn-ssh"
                      :font '("-fa" "Monospace" "-fs" "6")))

(defcommand mjru-xpanes-dh-ssh () ()
  (term-shell-command (join `("xpanes -t -c 'ssh {}.intr'" ,@mjru-dh))
                      :title "xpanes-dh-ssh"
                      :font '("-fa" "Monospace" "-fs" "6")))

(defcommand mjru-xpanes-web-ssh () ()
  (term-shell-command (join `("xpanes -t -c 'ssh {}.intr'" ,@mjru-webs))
                      :title "xpanes-web-ssh"
                      :font '("-fa" "Monospace" "-fs" "6")))

(defcommand mjru-xpanes-web-top () ()
  (term-shell-command (join `("xpanes -c 'ssh -t {}.intr top'" ,@mjru-webs))
                      :title "xpanes-web-top"
                      :font '("-fa" "Monospace" "-fs" "6")))

(defcommand mjru-xpanes-ssh-nginx () ()
  (term-shell-command (join `(,(xpanes-command "ssh -t {}")
                               ,@'("nginx1-mr.intr" "nginx2-mr.intr")))
                      :title "xpanes-routers"))

(defcommand mjru-xpanes-ssh-ns () ()
  (term-shell-command (join `(,(xpanes-command "ssh -t {}")
                               ,@'("ns1-mr.intr" "ns2-mr.intr" "ns1-dh.intr" "ns2-dh.intr")))
                      :title "xpanes-routers"))

(defcommand mjru-xpanes-routers () ()
  (term-shell-command (join `(,(xpanes-command "ssh -t {}")
                               ,@'("router4.intr" "vpn-miran.majordomo.ru" "vpn-dh.majordomo.ru")))
                      :title "xpanes-routers"))


;;;
;;; CI
;;;

(defvar *mjru-jenkins-url*
  "https://jenkins.intr")

(defcommand mjru-jenkins-webservices () ()
  (browse-url-firefox (format nil "~a/job/webservices" *mjru-jenkins-url*) t))

(define-key *top-map* (kbd "M-s-W") "mjru-jenkins-webservices")

(defcommand mjru-jenkins-group (group) ((:string "Group: "))
  (browse-url-firefox (format nil "~a/job/~a" *mjru-jenkins-url* group) t))


;;;
;;; Emacs
;;;

(defcommand mjru-servers () ()
  (progn (run-shell-command "emacsclient --eval '(wi-installed-servers)'")
         (switch-to-emacs)))


;;;
;;; MySQL
;;;

(defcommand mjru-xpanes-web-mycli () ()
  (term-shell-command (join `(,(xpanes-command
                                (format nil "mycli --password ~a -d {}"
                                        (password-store-show "majordomo/private/web/mysql/root")))
                               ,@mjru-webs))
                      :title "xpanes-web-mycli"
                      :font '("-fa" "Monospace" "-fs" "6")))


;;;
;;; Autotype
;;;

(defcommand mjru-pass-eng () ()
  (if (y-or-n-p "Insert eng password and press Enter? ")
      (window-send-string
       (format nil "~a~%" (password-store-show "majordomo/private/ssh/eng")))))

(defcommand mjru-pass-sup () ()
  (if (y-or-n-p "Insert sup password and press Enter? ")
      (window-send-string
       (format nil "~a~%" (password-store-show "majordomo/private/ssh/sup")))))

(defcommand mjru-pass-route () ()
  (if (y-or-n-p "Insert router password and press Enter? ")
      (window-send-string
       (format nil "~a~%" (password-store-show "majordomo/private/ssh/router")))))

(defcommand mjru-pass-ipmi () ()
  (if (y-or-n-p "Insert IPMI ADMIN password and press Enter? ")
      (window-send-string
       (format nil "~a" (password-store-show "majordomo/private/ipmi/ADMIN")))))

(defcommand mjru-pass-gitlab-ssh () ()
  (if (y-or-n-p "Insert GitLab SSH password and press Enter? ")
      (window-send-string
       (format nil "~a" (password-store-show "majordomo/private/gitlab.intr/ssh/id_rsa_gitlab_intr")))))

(defcommand mjru-pass-vnc () ()
  (if (y-or-n-p "Insert VNC SSH password and press Enter? ")
      (window-send-string
       (format nil "~a" (password-store-show "majordomo/public/kvm/vnc")))))


;;;
;;; Docker
;;;

(defcommand mjru-docker-pull () ()
  (window-send-string (format nil "~a~%" "docker ps --format '{{ .Image }}' | grep master | sort -u | xargs -I{} docker pull {}")))

(defcommand docker-firefox-esr-52 () ()
  (mapcar (lambda (command)
            (run-shell-command command))
          (list "xhost +local:"
                (join (list "docker" "run"
                            "--network=host" "--rm"
                            "--volume" "/etc/localtime:/etc/localtime:ro"
                            "--volume" "/tmp/.X11-unix:/tmp/.X11-unix"
                            "--user" "1000:997"
                            "--env" "DISPLAY=$DISPLAY"
                            "docker-registry.intr/utils/nix-docker-firefox-esr:master")))))


;;;
;;; Office
;;;

(defun mjru-office-shedule-open (url)
  (run-shell-command (format nil "firefox --new-window ~a" url))
  ;; TODO: Wait for window appear
  (sb-thread:make-thread
   (lambda ()
     (sleep 3) ;Wait for password insert on "Office.majordomo - Mozilla Firefox".
     (when (string= (window-title (current-window)) (format nil "Страница пользователя — ~a" *browser-name*))
       (run-commands "delete")
       (run-shell-command (format nil "firefox --new-window ~a" url))))))

(defcommand mjru-office-shedule-eng () ()
  (mjru-office-shedule-open "https://office.majordomo.ru/shedule2/10"))

(defcommand mjru-office-shedule-sup () ()
  (mjru-office-shedule-open "https://office.majordomo.ru/shedule2/2"))


;;;
;;; Console
;;;

(defcommand mjru-connect-br1-mr14.intr () ()
  (sb-thread:make-thread
   (lambda ()
     (sb-ext:run-program "/home/oleg/.guix-profile/bin/emacs"
                         `("--name=emacs@br1-mr14.intr" "--eval" "(mjru-connect-br1-mr14.intr)")
                         :input nil
                         :output *standard-output*))))


;;;
;;; Rofi
;;;

(defcommand rofi-billing2-server () ()
  "Open Rofi billing2 server."
  (let ((rofi-script (concat (getenv "HOME") "/.local/bin/rofi-billing2-server")))
    (run-rofi* (list "-modi" (concat "billing2:" rofi-script)
                     "-show" "billing2"))))


;;;
;;; VPN
;;;

(defvar *mjru-tapvpn-ip* "")

(defcommand mjru-ip-address-vpn-update () ()
  (setq *mjru-tapvpn-ip* (network-address "tapvpn")))
