;; Thanks
;; https://lists.gnu.org/archive/html/help-guix/2016-01/msg00064.html

(define-module (sysadmin services)
  #:use-module ((guix ui) #:select (make-user-module))
  #:use-module (gnu packages android)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu services certbot)
  #:use-module (gnu services cups)
  #:use-module (gnu services desktop)
  #:use-module (gnu services mail)
  #:use-module (gnu services networking)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services ssh)
  #:use-module (gnu services sysctl)
  #:use-module (gnu services version-control)
  #:use-module (gnu services xorg)
  #:use-module (gnu)
  #:use-module (guix profiles)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (custom-packages

            proxy

            prefix-local-host-aliases
            serialize-hosts

            letsencrypt-certificate
            letsencrypt-key

            %nginx-deploy-hook

            custom-desktop-services
            %setuid-custom-programs))


;;;
;;; Utils
;;;

(define (cartesian-product . lists)
  (fold-right (lambda (xs ys)
                (append-map (lambda (x)
                              (map (lambda (y)
                                     (cons x y))
                                   ys))
                            xs))
              '(())
              lists))

(define* (prefix-local-host-aliases
          #:key prefixes host-name domain ip-addresses)
  (string-join (map (lambda (x)
                      (string-append (string-join x " ")
                                     "." host-name domain))
                    (cartesian-product ip-addresses prefixes))
               "\n"))

(define (serialize-hosts lst)
  (string-join (map (match-lambda
                      ((ip-address . canonical-hostname)
                       (format #f "~a ~a"
                               ip-address canonical-hostname)))
                    lst)
               "\n"))


;;;
;;; Certbot
;;;

(define letsencrypt-certificate
  (cut string-append "/etc/letsencrypt/live/" <> "/fullchain.pem"))

(define letsencrypt-key
  (cut string-append "/etc/letsencrypt/live/" <> "/privkey.pem"))


;;;
;;; Guix
;;;

(define %guix-daemon-config
  (guix-configuration
   ;; Import keys
   ;; $ wget https://git.savannah.gnu.org/cgit/guix/maintenance.git/plain/hydra/keys/guix/berlin.guixsd.org-export.pub
   ;; # “guix archive --authorize < berlin.guixsd.org-export.pub”
   (substitute-urls '("https://berlin.guixsd.org"

                      ;; TODO: Uncomment when substitute servers become online
                      ;; "https://mirror.hydra.gnu.org"
                      ;; "https://hydra.gnu.org"
                      ))
   ;; (authorized-keys '())
   (max-silent-time 7200)
   (timeout (* 4 max-silent-time))

   (extra-options '("--max-jobs=6" "--cores=3"
                    ;; Origin <https://lists.gnu.org/archive/html/guix-devel/2018-07/msg00310.html>.
                    "--gc-keep-derivations=yes"
                    "--gc-keep-outputs=yes"))))


;;;
;;; Firewall service
;;;

(define start-firewall
  #~(let ((iptables
           (lambda (str)
             (zero? (system (string-join `(,#$(file-append iptables
                                                           "/sbin/iptables")
                                           ,str) " "))))))
      (format #t "Install iptables rules.~%")
      (and
       ;; Rules to throttle malicious SSH connection attempts.  This will
       ;; allow at most 3 connections per minute from any host, and will block
       ;; the host for another minute if this rate is exceeded.  Taken from
       ;; <http://www.la-samhna.de/library/brutessh.html#3>.
       #;(iptables "-A INPUT -p tcp --dport 22 -m state \
  --state NEW -m recent --set --name SSH -j ACCEPT")
       #;(iptables "-A INPUT -p tcp --dport 22 -m recent \
  --update --seconds 60 --hitcount 4 --rttl \
  --name SSH -j LOG --log-prefix SSH_brute_force")
       #;(iptables "-A INPUT -p tcp --dport 22 -m recent \
  --update --seconds 60 --hitcount 4 --rttl --name SSH -j DROP")

       ;; TODO: Map over a list of ports
       ;; (iptables "-A INPUT -p tcp --dport 80 -s 192.168.0.0/16 -j ACCEPT") ; web
       ;; (iptables "-A INPUT -p tcp --dport 445 -s 192.168.0.0/16 -j ACCEPT") ; smb
       ;; (iptables "-A INPUT -p tcp --dport 3389 -s 192.168.0.0/16 -j ACCEPT") ; rdp
       ;; (iptables "-A INPUT -p tcp --dport 80 -s 127.0.0.0/8 -j ACCEPT")
       ;; (iptables "-A INPUT -p tcp --dport 80 -j DROP")

       ;; Rules to throttle HTTP connection redirections.  Taken from
       ;; <https://www.opennet.ru/tips/2999_iptables_block_tor.shtml>.
       (iptables "-A INPUT -p tcp --sport 443 --tcp-flags RST RST -j DROP")
       (iptables "-A INPUT -p tcp --sport 80 -m string \
--string \"Location: http://warning.rt.ru\" --algo bm -j DROP")
       (iptables "-A INPUT -p tcp --sport 80 -m string \
--string \"Location: http://promo.nw.rt.ru\" --algo bm -j DROP"))))

(define firewall-service
  ;; The "firewall".  Make it a Shepherd service because as an activation
  ;; script it might run too early, before the Netfilter modules can be
  ;; loaded for some reason.
  (simple-service 'firewall shepherd-root-service-type
                  (list
                   (shepherd-service
                    (provision '(firewall))
                    (requirement '())
                    (start #~(lambda _
                               #$start-firewall))
                    (respawn? #f)
                    (stop #~(lambda _
                              (zero?
                               (system* #$(file-append iptables
                                                       "/sbin/iptables")
                                        "-F"))))))))


;;;
;;; NGINX
;;;

(define* (proxy host port #:optional (protocol "http"))
    (list "resolver 192.168.105.120;"
          (string-append "set $target localhost:" (number->string port) ";")
          (format #f "proxy_pass ~a://$target;" protocol)
          (format #f "proxy_set_header Host ~a;" host)
          "proxy_set_header X-Real-IP $remote_addr;"
          "proxy_set_header X-Forwarded-for $remote_addr;"
          "proxy_connect_timeout 300;"))

(define %nginx-deploy-hook
  (program-file
   "nginx-deploy-hook"
   #~(let ((pid (call-with-input-file "/var/run/nginx/pid" read)))
       (kill pid SIGHUP))))


;;;
;;; Desktop
;;;

(define 20-intel.conf "
# Fix tearing on intel
# https://wiki.archlinux.org/index.php/Intel_Graphics
# https://github.com/8p8c/my-guix/blob/master/config.scm
Section \"Device\"
   Identifier  \"Intel Graphics\"
   Driver      \"intel\"
   Option      \"AccelMethod\"  \"sna\"
   Option      \"SwapbuffersWait\" \"true\"
   Option      \"TearFree\" \"true\"
EndSection
")

(define %setuid-custom-programs
  (cons (file-append fping "/sbin/fping")
        %setuid-programs))

(define (custom-packages file)
  (let ((module (make-user-module '((guix profiles) (gnu)))))
    (save-module-excursion
     (lambda _
       (set-current-module module)
       (map manifest-entry-item
            (manifest-entries (load file)))))))

(define* (custom-desktop-services #:key tor-config-file)
  (cons* 
   firewall-service

   ;; 92 bytes from switch.local (192.168.105.1): Redirect Host    
   (service sysctl-service-type
            (sysctl-configuration
             (settings '(("net.ipv4.ip_forward" . "1")
                         ("net.ipv4.conf.all.accept_redirects" . "1")
                         ("net.ipv4.conf.all.send_redirects" . "1")))))

   (service openssh-service-type (openssh-configuration (permit-root-login #t)
                                                        (x11-forwarding? #t)))

   (service cups-service-type
            (cups-configuration
             (location-access-controls
              (list (location-access-control
                     (path "/")
                     (access-controls '("Order allow,deny"
                                        "Allow localhost"
                                        "Allow 192.168.0.*")))
                    (location-access-control
                     (path "/admin")
                     (access-controls '("Order allow,deny"
                                        "Allow localhost")))
                    (location-access-control
                     (path "/admin/conf")
                     (access-controls '("Order allow,deny"
                                        "AuthType Basic"
                                        "Require user @SYSTEM"
                                        "Allow localhost")))))
             (web-interface? #t) ; LibreJS could block JS
             (extensions (list cups-filters hplip-minimal))))

   (dovecot-service #:config (dovecot-configuration
                              (listen '("127.0.0.1"))
                              (disable-plaintext-auth? #f)
                              (mail-location
                               (string-append "maildir:~/Maildir"
                                              ":INBOX=~/Maildir/INBOX"
                                              ":LAYOUT=fs"))))

   (service git-daemon-service-type
            (git-daemon-configuration (user-path "")
                                      (export-all? #t)))

   (tor-service (local-file tor-config-file))

   ;; “adb” and “fastboot” without root privileges
   (simple-service 'adb udev-service-type (list android-udev-rules))

   (xfce-desktop-service)

   (modify-services (remove (lambda (service)
                              ;; Remove NetworkManager
                              (or (eq? (service-kind service)
                                       network-manager-service-type)))
                            %desktop-services)

     (guix-service-type config => %guix-daemon-config)

     (slim-service-type config => (slim-configuration
                                   (inherit config)
                                   (startx (xorg-start-command
                                            #:configuration-file (xorg-configuration-file
                                                                  #:extra-config (list 20-intel.conf)))))))))
