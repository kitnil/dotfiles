(define-module (wigust packages tome4)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages)
  #:use-module (gnu packages games)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (tome4 packages tome4))

(define-public tome4-with-addons-custom
  (package
    (inherit tome4-with-addons)
    (name "tome4-with-addons-custom")
    (version (package-version tome4))
    (arguments
     (substitute-keyword-arguments
         (package-arguments tome4-with-addons)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-before 'install 'my-customizations
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((unzip (string-append (assoc-ref inputs "unzip") "/bin/unzip"))
                     (zip (string-append (assoc-ref inputs "zip") "/bin/zip")))
                 (let ((archive (string-append "game/modules/tome-" ,version ".team")))
                   (mkdir-p "mod/class")
                   (system* unzip "-j" archive "mod/class/Game.lua" "-d" "mod/class")
                   ;; Change tactic grid color from yellow to black.
                   (substitute* "mod/class/Game.lua"
                     (("d5990880") "1a1a1a80"))
                   (system* zip archive "mod/class/Game.lua")))))))))))
