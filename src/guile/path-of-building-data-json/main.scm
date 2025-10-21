#!/run/current-system/profile/bin/guile \
--no-auto-compile -e main -s
!#

(use-modules (ice-9 format)
             (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 string-fun)
             (srfi srfi-1)

             (json)

             (guix records))

(define-record-type* <rule>
  rule make-rule
  rule?
  (base-type rule-base-type ;string
             (default #f))
  (item-level rule-item-level ;number
              (default #f)))

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
                                     (let ((item-name (string-replace-substring (first item)
                                                                                (format #f " (~a)" base-type)
                                                                                "")))
                                       ;; (pk (rule (base-type item-name)))
                                       item-name))
                                   items)
                              string<))
                (format #t "\tSetFontSize ~a~%" font-size)))
            '("Armour/Evasion"
              "Evasion"))

  (define item-level 82)
  (newline)
  (format #t "~a~%" "Show")
  (format #t "\tItemLevel >= ~a~%" item-level)
  (let ((red 74)
        (green 230)
        (blue 58)
        (alpha 255))
    (format #t "\tSetBorderColor ~a ~a ~a ~a~%" red green blue alpha))
  (format #t "\tContinue"))
