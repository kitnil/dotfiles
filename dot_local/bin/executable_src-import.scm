#!/run/current-system/profile/bin/guile \
--no-auto-compile -e (src-import) -s
!#

(define-module (src-import)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-26)
  #:use-module (guix records)
  #:use-module (ice-9 pretty-print)
  #:export (main))

(define (project-directories basedir)
  "Return a list of Git projects excluding worktrees in BASEDIR."
  (let ((scandir-predicate (lambda (file)
                             (and (not (string= file "."))
                                  (not (string= file ".."))))))
    (sort (filter
           (lambda (project-directory)
             (and (match (scandir project-directory
                                  (lambda (file)
                                    (string= file ".git")))
                    ((".git") #t)
                    (_ #f))
                  (match (scandir (string-append project-directory "/.git")
                                  (lambda (file)
                                    (string= file "config")))
                    (("config") #t)
                    (_ #f))))
           (apply append
                  (fold (lambda (group groups)
                          (let ((group-directory (string-append basedir "/" group)))
                            (cons (map (lambda (project)
                                         (string-append group-directory "/" project))
                                       (scandir group-directory scandir-predicate))
                                  groups)))
                        '()
                        (scandir basedir scandir-predicate))))
          string<)))

(define %basedir
  (and=> (getenv "HOME")
         (lambda (home)
           (string-append home "/majordomo"))))

(define (main args)
  (define projects
    (project-directories %basedir))

  (for-each (lambda (project)
              (pretty-print project #:width 10)
              (newline))
            (map (lambda (project)
                   (match (string-split (string-replace-substring
                                         project (string-append %basedir "/") "")
                                        #\/)
                     ((group name)
                      (let ((group (match group
                                     ("_ci" "ci")
                                     (group group))))
                        `(define-public ,(string->symbol
                                          (string-append "git-project-" group "-" name))
                           (git-project
                            (name ,name)
                            (group ,group)
                            (output ,project)))))))
                 projects)))
