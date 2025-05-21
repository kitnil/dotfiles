(use-modules (sxml simple))

(define %qterminal-config
  (and=> (getenv "HOME")
         (lambda (home)
           (string-append home "/.config/qterminal.org/qterminal_bookmarks.xml"))))

(with-output-to-file %qterminal-config
  (lambda ()
    (sxml->xml '(qterminal (group (@ (name "ssh"))
                                  (command (@ (value "ssh workstation.intr")
                                              (name "workstation")))
                                  (command (@ (value "ssh spb")
                                              (name "spb"))))))))
