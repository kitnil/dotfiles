#!/run/current-system/profile/bin/guile \
--no-auto-compile -e main -s
!#

(use-modules (ice-9 format)
             (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 string-fun)
             (srfi srfi-1)

             (json))

(define (main . args)
  (define font-size 20)
  (for-each (lambda (base-type)
              (let ((items
                     (filter (lambda (item)
                               (and=> (assoc-ref item "subType")
                                      (lambda (sub-type)
                                        (string= sub-type base-type))))
                             (json-string->scm (with-input-from-file "input.json" read-string)))))
                (format #t "~%# ~a~%" base-type)
                (format #t "~a~%" "Show")
                (format #t "\tBaseType == ~{ ~s~}~%"
                        (sort (map (lambda (item)
                                     (string-replace-substring (first item)
                                                               (format #f " (~a)" base-type)
                                                               ""))
                                   items)
                              string<))
                (format #t "\tSetFontSize ~a~%" font-size)))
            '("Armour/Evasion"
              "Evasion")))
