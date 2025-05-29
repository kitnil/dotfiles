#!/usr/bin/env -S guile --no-auto-compile -e main -s
!#

(define-module (wugi scripts clone-gitlab-intr)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:export (main))

(define gitlab-url
  (and=> (getenv "GITLAB_URL")
         (lambda (gitlab-url)
           gitlab-url)))

(define gitlab-token
  (and=> (getenv "GITLAB_TOKEN")
         (lambda (gitlab-token)
           gitlab-token)))

(define (main . args)
  (let* ((port (apply open-pipe* OPEN_READ
                      (list "curl"
                            "--header" (format #f "PRIVATE-TOKEN: ~a" gitlab-token)
                            "--header" "Content-Type: application/json"
                            "--silent"
                            "--insecure"
                            "--request" "GET"
                            (string-append gitlab-url "/api/v4/groups"))))
         (output (read-string port)))
    (close-port port)
    (map (lambda (group)
           (let* ((port (apply open-pipe* OPEN_READ
                               (list "curl"
                                     "--header" (format #f "PRIVATE-TOKEN: ~a" gitlab-token)
                                     "--header" "Content-Type: application/json"
                                     "--silent"
                                     "--insecure"
                                     "--request" "GET"
                                     (string-append gitlab-url "/api/v4/groups/"
                                                    (number->string (assoc-ref group "id"))))))
                  (output (read-string port)))
             (close-port port)
             (unless (file-exists? (assoc-ref group "name"))
               (mkdir (assoc-ref group "full_path")))
             (for-each (lambda (project)
                         (system* "git" "clone"
                                  (assoc-ref project "ssh_url_to_repo")
                                  (string-append (assoc-ref group "full_path")
                                                 "/"
                                                 (assoc-ref project "path"))))
                       (array->list (assoc-ref (json-string->scm output) "projects"))))
           (assoc-ref group "full_path"))
         (array->list (json-string->scm output)))))
