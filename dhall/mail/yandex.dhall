let Mail
    : Type
    = { from : Text, password : Text }

in    λ(mail : Mail)
    → ''
      set realname = "zabbix.guix"
      set from = ${mail.from}
      set use_from = yes
      set imap_user = ${mail.from}
      set imap_pass = {{ pass "${mail.password}" }}
      set smtp_url = smtp://smtp.yandex.ru:465/
      set smtp_pass = {{ pass "${mail.password}" }}
      set smtp_authenticators = 'login'
      set ssl_starttls = yes
      set ssl_force_tls = yes
      set folder = imaps://imap.yandex.ru:993
      set spoolfile = +INBOX
      set record = =Отправленные
      ''
