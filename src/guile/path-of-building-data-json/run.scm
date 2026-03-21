#!/run/current-system/profile/bin/guile \
--no-auto-compile -e (run) -s
!#

(define-module (run)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:export (main))

(define %options
  (let ((display-and-exit-proc (lambda (msg)
                                 (lambda (opt name arg loads)
                                   (display msg) (quit)))))
    (list (option '(#\v "version") #f #f
                  (display-and-exit-proc "run version 0.0.1\n"))
          (option '(#\h "help") #f #f
                  (display-and-exit-proc
                   "Usage: run ...")))))

(define %default-options
  '())

(define poe-filter-script
  "./poe-filter.scm")

(define (main args)
  (define opts
    (args-fold (cdr (program-arguments))
               %options
               (lambda (opt name arg loads)
                 (error "Unrecognized option `~A'" name))
               (lambda (op loads)
                 (cons op loads))
               %default-options))
  (define output-file
    (cut string-append
         "/mnt/steam/home/oleg/.local/share/Steam/steamapps/compatdata/238960/pfx/drive_c/users/steamuser/Documents/My Games/Path of Exile/" <>))
  (define duelist-args
    (append (map (lambda (defence)
                   (string-append "--defence=" defence))
                 '("Armour"
                   "Armour/Evasion"))

            (map (lambda (defence)
                   (string-append "--exclude-defence=" defence))
                 '("Armour/Energy Shield"
                   "Evasion/Energy Shield"
                   "Energy Shield"))

            (map (lambda (weapon)
                   (string-append "--exclude-weapon=" weapon))
                 '("Corpses"
                   "Sceptres"
                   "Staves"
                   "Wands"
                   "Warstaves"))

            (list "--ruthless"
                  (string-append "--output=" (output-file "duelist.ruthlessfilter")))))
  (define shadow-args
    (append (map (lambda (defence)
                   (string-append "--defence=" defence))
                 '("Evasion/Energy Shield"
                   "Evasion"
                   "Energy Shield"))

            (map (lambda (defence)
                   (string-append "--exclude-defence=" defence))
                 '("Armour"
                   "Armour/Evasion"
                   "Armour/Energy Shield"))

            (map (lambda (weapon)
                   (string-append "--exclude-weapon=" weapon))
                 '("Corpses"
                   "One Hand Axes"
                   "One Hand Maces"
                   "Sceptres"
                   "Staves"
                   "Two Hand Axes"
                   "Two Hand Maces"
                   "Wands"
                   "Warstaves"))

            (list "--ruthless"
                  "--backup"
                  (string-append "--output=" (output-file "shadow.ruthlessfilter")))))
  (for-each (lambda (args)
              (apply system* (append (list poe-filter-script) args)))
            (list duelist-args
                  shadow-args)))
