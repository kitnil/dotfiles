(define-module (guile mariadb)
  #:use-module (guix records)
  #:export (mariadb-connection
            mariadb-connection?
            mariadb-connection-statement
            <mariadb-connection>))

(define-record-type* <mariadb-connection>
  mariadb-connection make-mariadb-connection
  mariadb-connection?
  (user      mariadb-connection-user       ;string
             (default #f))
  (password  mariadb-connection-password   ;string
             (default #f))
  (host      mariadb-connection-host       ;string
             (default #f))
  (database  mariadb-connection-database   ;string
             (default #f))
  (statement mariadb-connection-statement  ;string
             (default #f))
  (arguments mariadb-connection-arguments  ;list of strings
             (default '("--skip-column-names"))))
