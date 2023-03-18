#!/usr/bin/env -S guile --no-auto-compile -e main -s
!#

;; GITLAB_URL=https://gitlab.softwareheritage.org

(use-modules (ice-9 format)
             (ice-9 match)
             (ice-9 popen)
             (ice-9 rdelim)
             (json)
             (srfi srfi-1)
             (ice-9 pretty-print)
             (guix build utils))

(define gitlab-url
  (and=> (getenv "GITLAB_URL")
         (lambda (gitlab-url)
           gitlab-url)))

(define gitlab-token
  (and=> (getenv "GITLAB_TOKEN")
         (lambda (gitlab-token)
           gitlab-token)))

(define (gitlab-groups)
  (read-string
   (apply open-pipe* OPEN_READ
          `("curl"
            ,@(if gitlab-token
                  (list "--header"
                        (format #f "PRIVATE-TOKEN: ~a" gitlab-token))
                  '())
            "--header" "Content-Type: application/json"
            "--silent"
            "--insecure"
            "--request" "GET"
            ,(string-append gitlab-url "/api/v4/groups")))))

(define groups
  #f)

(define gitlab-subgroups
  #t)

(define (main . args)
  (map (lambda (group)
         (let* ((port (apply open-pipe* OPEN_READ
                             `("curl"
                               ,@(if gitlab-token
                                     (list "--header" (format #f "PRIVATE-TOKEN: ~a" gitlab-token))
                                     '())
                               "--header" "Content-Type: application/json"
                               "--silent"
                               "--insecure"
                               "--request" "GET"
                               ,(string-append gitlab-url "/api/v4/groups/"
                                               (number->string (assoc-ref group "id"))
                                               (if gitlab-subgroups
                                                   "/subgroups"
                                                   "")))))
                (output (read-string port)))
           (close-port port)

           (for-each (lambda (subgroup)
                       (let* ((port (apply open-pipe* OPEN_READ
                                           `("curl"
                                             ,@(if gitlab-token
                                                   (list "--header" (format #f "PRIVATE-TOKEN: ~a" gitlab-token))
                                                   '())
                                             "--header" "Content-Type: application/json"
                                             "--silent"
                                             "--insecure"
                                             "--request" "GET"
                                             ,(string-append gitlab-url "/api/v4/groups/"
                                                             (number->string (assoc-ref subgroup "id"))))))
                              (output (read-string port)))
                         (for-each (lambda (project)
                                     (invoke "git" "grab"
                                             (assoc-ref project "http_url_to_repo")))
                                   (array->list (assoc-ref (json-string->scm output) "projects")))
                         (close-port port)))
                     (array->list (json-string->scm output)))

           (for-each (lambda (subgroup)
                       (let* ((port (apply open-pipe* OPEN_READ
                                           `("curl"
                                             ,@(if gitlab-token
                                                   (list "--header" (format #f "PRIVATE-TOKEN: ~a" gitlab-token))
                                                   '())
                                             "--header" "Content-Type: application/json"
                                             "--silent"
                                             "--insecure"
                                             "--request" "GET"
                                             ,(string-append gitlab-url "/api/v4/groups/"
                                                             (number->string (assoc-ref subgroup "id"))
                                                             (if gitlab-subgroups
                                                                 "/subgroups"
                                                                 "")))))
                              (output (read-string port)))
                         (for-each (lambda (subgroup)
                                     (let* ((port (apply open-pipe* OPEN_READ
                                                         `("curl"
                                                           ,@(if gitlab-token
                                                                 (list "--header" (format #f "PRIVATE-TOKEN: ~a" gitlab-token))
                                                                 '())
                                                           "--header" "Content-Type: application/json"
                                                           "--silent"
                                                           "--insecure"
                                                           "--request" "GET"
                                                           ,(string-append gitlab-url "/api/v4/groups/"
                                                                           (number->string (assoc-ref subgroup "id"))))))
                                            (output (read-string port)))
                                       (for-each (lambda (project)
                                                   (invoke "git" "grab"
                                                           (assoc-ref project "http_url_to_repo")))
                                                 (array->list (assoc-ref (json-string->scm output) "projects")))
                                       (close-port port)))
                                   (array->list (json-string->scm output)))
                         (close-port port)))
                     (array->list (json-string->scm output)))))
       (if groups
           (array->list (json-string->scm (gitlab-groups)))
           '((("id" . 25))))))
