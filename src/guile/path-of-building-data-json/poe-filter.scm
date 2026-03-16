#!/run/current-system/profile/bin/guile \
--no-auto-compile -e (poe-filter) -s
!#

(define-module (poe-filter)
  #:use-module (gnu services configuration)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix scripts)
  #:use-module (guix store)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 string-fun)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-171)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:export (main))

(define %options
  (list (option '(#\D "exclude-defence") #f #t
                (lambda (opt name arg result)
                  (alist-cons 'exclude-defence arg result)))
        (option '(#\d "defence") #f #t
                (lambda (opt name arg result)
                  (alist-cons 'defence arg result)))
        (option '(#\W "exclude-weapon") #f #t
                (lambda (opt name arg result)
                  (alist-cons 'exclude-weapon arg result)))
        (option '(#\r "ruthless") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'ruthless? #t result)))
        (option '(#\o "output") #f #t
                (lambda (opt name arg result)
                  (alist-cons 'output arg result)))))

(define (uglify-field-name field-name)
  (apply string-append
         (map string-titlecase
              (string-split (string-delete #\?
                                           (symbol->string field-name))
                            #\-))))

(define show?
  boolean?)

(define (serialize-show field value)
  (format #f "~a~%" (if value "Show" "Hide")))

(define (serialize-string field-name value)
  (format #f "~a ~a~%" field-name value))

(define (serialize-list-of-strings field-name value)
  (string-append (map (cut serialize-string field-name <>) value)))

(define-maybe list-of-strings)

(define (serialize-symbol-1 field-name value)
  (symbol->string value))

(define (serialize-list-of-symbols field-name value)
  (if (null? value)
      ""
      (string-append "\t"
                     (string-join (append (list (uglify-field-name field-name))
                                          (map (lambda (v)
                                                 (symbol->string v))
                                               value)))
                     "\n")))

(define-maybe list-of-symbols)

(define set-font-size? integer?)

(define (serialize-set-font-size field-name value)
  (if (= value 0)
      ""
      (format #f "\t~a ~a~%" (uglify-field-name field-name) value)))

(define base-types?
  list-of-strings?)

(define (serialize-base-types field-name value)
  (if (null? value)
      ""
      (if (> (length value) 1)
          (format #f "\tBaseType == ~{~s ~}~%" value)
          (format #f "\tBaseType ~{~s ~}~%" value))))

(define classes?
  list-of-strings?)

(define (serialize-classes field-name value)
  (if (null? value)
      ""
      (format #f "\tClass == ~{~s ~}~%" value)))

(define commentary? string?)

(define (serialize-commentary field-name value)
  (if (string-null? value)
      ""
      (format #f "# ~a~%" value)))

(define (operator? x)
  (or (boolean? x)
      (member x '(= ;Equal
                  ! ;Not equal
                  != ;Not equal
                  <= ;Less than or equal
                  >= ;Greater than or equal
                  < ;Less than
                  > ;Greater than
                  == ;Exact match
                  ))))

(define (serialize-operator field-name value)
  (symbol->string value))

(define (conditional-value-1? x)
  (or (number? x)
      (symbol? x)))

(define (serialize-conditional-value-1 field-name value)
  (cond ((number? value)
         (number->string value))
        ((symbol? value)
         (symbol->string value))))

(define-configuration poe-item-filter-conditional-value-configuration
  (operator
   (operator #f)
   "")
  (value
   (conditional-value-1 1)
   ""))

(define (serialize-conditional-value-configuration config fields)
  "Similar to serialize-configuration."
  (string-join
   `(,@(list-transduce (base-transducer config) rcons fields))))

(define conditional-value? poe-item-filter-conditional-value-configuration?)
(define (serialize-conditional-value field-name value)
  (if (poe-item-filter-conditional-value-configuration-operator value)
      (format #f
              "\t~a ~a~%"
              (uglify-field-name field-name)
              (serialize-conditional-value-configuration value poe-item-filter-conditional-value-configuration-fields))
      ""))

(define continue? boolean?)

(define (serialize-continue field-name value)
  (if value
      (string-append "\t" (uglify-field-name field-name) "\n")
      ""))

(define (serialize-integer field-name value)
  (value))

(define-configuration poe-item-filter-color-configuration
  ;; XXX: Refactor poe-item-filter-color-configuration less than 256 hack.
  (red
   (integer 256)
   "")
  (green
   (integer 256)
   "")
  (blue
   (integer 256)
   "")
  (alpha
   (integer 256)
   ""))

(define (color? x)
  (or (boolean? x)
      (poe-item-filter-color-configuration? x)))

(define (serialize-color field-name value)
  ;; XXX: Refactor poe-item-filter-color-configuration less than 256 hack.
  (if (or (and (or (> (poe-item-filter-color-configuration-red value) 0)
                   (> (poe-item-filter-color-configuration-green value) 0)
                   (> (poe-item-filter-color-configuration-blue value) 0)
                   (> (poe-item-filter-color-configuration-alpha value) 0))
               (or (< (poe-item-filter-color-configuration-red value) 256)
                   (< (poe-item-filter-color-configuration-green value) 256)
                   (< (poe-item-filter-color-configuration-blue value) 256)
                   (< (poe-item-filter-color-configuration-alpha value) 256)))
          (or (= (poe-item-filter-color-configuration-red value) 0)
              (= (poe-item-filter-color-configuration-green value) 0)
              (= (poe-item-filter-color-configuration-blue value) 0)
              (= (poe-item-filter-color-configuration-alpha value) 0)))
      (string-append "\t"
                     (string-join (list (uglify-field-name field-name)
                                        (number->string (poe-item-filter-color-configuration-red value))
                                        (number->string (poe-item-filter-color-configuration-green value))
                                        (number->string (poe-item-filter-color-configuration-blue value))
                                        (number->string (poe-item-filter-color-configuration-alpha value))))
                     "\n")
      ""))

(define (serialize-symbol field-name value)
  value)

(define (serialize-boolean field-name value)
  value)

(define-configuration poe-item-filter-minimap-icon-configuration
  (enabled?
   (boolean #f)
   "")
  (size
   (integer 0)
   "")
  (colour
   (symbol 'White)
   "")
  (shape
   (symbol 'Circle)
   ""))

(define minimap-icon?
  poe-item-filter-minimap-icon-configuration?)

(define (serialize-minimap-icon field-name value)
  (if (poe-item-filter-minimap-icon-configuration-enabled? value)
      (string-append "\t"
                     (string-join (list (uglify-field-name field-name)
                                        (number->string (poe-item-filter-minimap-icon-configuration-size value))
                                        (symbol->string (poe-item-filter-minimap-icon-configuration-colour value))
                                        (symbol->string (poe-item-filter-minimap-icon-configuration-shape value))))
                     "\n")
      ""))

(define (boolean-with-field? x)
  (or (and (symbol? x)
           (eq? 'disabled x))
      (boolean? x)))

(define (serialize-boolean-with-field field-name value)
  (if (and (symbol? value)
           (eq? 'disabled value))
      ""
      (string-append "\t"
                     (string-join (list (uglify-field-name field-name)
                                        (if value "True" "False")))
                     "\n")))

(define-configuration poe-item-filter-play-alert-sound-configuration
  (id
   (integer 1)
   "")
  (volume
   (integer 50)
   ""))

(define (play-alert-sound? x)
  (or (boolean? x)
      (poe-item-filter-play-alert-sound-configuration? x)))

(define (serialize-play-alert-sound field-name value)
  (if value
      (format #f "\t~a ~a ~a~%"
              (uglify-field-name field-name)
              (poe-item-filter-play-alert-sound-configuration-id value)
              (poe-item-filter-play-alert-sound-configuration-volume value))
      ""))

(define (symbol-or-false? x)
  (or (symbol? x) (boolean? x)))

(define (serialize-symbol-or-false field-name value)
  (if value
      (symbol->string value)
      ""))

(define-configuration poe-item-filter-play-effect-configuration
  (colour
   (symbol 'Red)
   "")
  (temp
   (symbol-or-false #f)
   ""))

(define (play-effect? x)
  (or (boolean? x)
      (poe-item-filter-play-effect-configuration? x)))

(define (serialize-play-effect field-name value)
  (if value
      (format #f "\t~a ~a~%"
              (uglify-field-name field-name)
              (poe-item-filter-play-effect-configuration-colour value)
              ;; (poe-item-filter-play-effect-configuration-temp value)
              )
      ""))

(define-configuration poe-item-filter-block-configuration
  (commentary
   (commentary "")
   "")
  (show?
   (show #t)
   "If any items are matched with the conditions in a \"Show\" Block, the item will be shown.")
  (item-level
   (conditional-value (poe-item-filter-conditional-value-configuration))
   "")
  (area-level
   (conditional-value (poe-item-filter-conditional-value-configuration))
   "")
  (stack-size
   (conditional-value (poe-item-filter-conditional-value-configuration))
   "")
  (quality
   (conditional-value (poe-item-filter-conditional-value-configuration))
   "")
  (memory-strands
   (conditional-value (poe-item-filter-conditional-value-configuration))
   "")
  (linked-sockets
   (conditional-value (poe-item-filter-conditional-value-configuration))
   "")
  (sockets
   (conditional-value (poe-item-filter-conditional-value-configuration))
   "")
  (socket-group
   (conditional-value (poe-item-filter-conditional-value-configuration))
   "")
  (gem-level
   (conditional-value (poe-item-filter-conditional-value-configuration))
   "")
  (base-defence-percentile
   (conditional-value (poe-item-filter-conditional-value-configuration))
   "")
  (base-types
   (base-types '())
   "")
  (classes
   (classes '())
   "")
  (minimap-icon
   (minimap-icon (poe-item-filter-minimap-icon-configuration))
   "")
  (set-font-size
   (set-font-size 0)
   "")
  (set-border-color
   (color (poe-item-filter-color-configuration))
   "")
  (set-background-color
   (color (poe-item-filter-color-configuration))
   "")
  (set-text-color
   (color (poe-item-filter-color-configuration))
   "")
  (rarity
   (list-of-symbols '())
   "")
  (play-alert-sound
   (play-alert-sound #f)
   "")
  (play-effect
   (play-effect #f)
   "")
  (identified?
   (boolean-with-field 'disabled)
   "")
  (fractured-item?
   (boolean-with-field 'disabled)
   "")
  (transfigured-gem?
   (boolean-with-field 'disabled)
   "")
  (continue?
   (continue #f)
   ""))

(define (blocks? lst)
  (every poe-item-filter-block-configuration? lst))

(define (serialize-blocks field-name value)
  #~(string-join (list #$@(map (lambda (v)
                                 (serialize-configuration v poe-item-filter-block-configuration-fields))
                               value))
                 "\n"))

(define-configuration poe-item-filter-configuration
  (commentary
   (commentary "")
   "")
  (blocks
   (blocks '())
   ""))

(define poe-filter-basic
  (poe-item-filter-block-configuration
   (commentary "Lower font for basic items.")
   (classes '("Helmets" "Body Armour" "Boots" "Gloves" "Shields"))
   (item-level (poe-item-filter-conditional-value-configuration
                (value 84)
                (operator '<)))
   (area-level (poe-item-filter-conditional-value-configuration
                (value 75)
                (operator '>)))
   (set-font-size 25)
   (continue? #t)))

(define poe-filter-crafting
  (poe-item-filter-block-configuration
   (commentary "Highlight border of best crafting bases.")
   (item-level (poe-item-filter-conditional-value-configuration
                (value 84)
                (operator '>=)))
   (set-border-color (poe-item-filter-color-configuration
                      (red 74)
                      (green 230)
                      (blue 58)
                      (alpha 255)))
   (continue? #t)))

(define poe-filter-quality-low-level
  (poe-item-filter-block-configuration
   (commentary "Highlight high quality items.")
   (quality (poe-item-filter-conditional-value-configuration
             (value 0)
             (operator '>)))
   (area-level (poe-item-filter-conditional-value-configuration
                (value 50)
                (operator '<)))
   (set-text-color
    (poe-item-filter-color-configuration
     (red 30) (green 190) (blue 190) (alpha 255)))
   (set-border-color
    (poe-item-filter-color-configuration
     (red 30) (green 190) (blue 190) (alpha 255)))
   (minimap-icon
    (poe-item-filter-minimap-icon-configuration
     (enabled? #t)
     (size 1)
     (colour 'White)
     (shape 'Triangle)))
   (play-alert-sound
    (poe-item-filter-play-alert-sound-configuration
     (id 2)
     (volume 300)))
   (play-effect
    (poe-item-filter-play-effect-configuration
     (colour 'Grey)))))

(define poe-filter-quality
  (poe-item-filter-block-configuration
   (commentary "Highlight high quality items.")
   (quality (poe-item-filter-conditional-value-configuration
             (value 20)
             (operator '>)))
   (set-text-color
    (poe-item-filter-color-configuration
     (red 30) (green 190) (blue 190) (alpha 255)))
   (set-border-color
    (poe-item-filter-color-configuration
     (red 30) (green 190) (blue 190) (alpha 255)))
   (minimap-icon
    (poe-item-filter-minimap-icon-configuration
     (enabled? #t)
     (size 1)
     (colour 'White)
     (shape 'Triangle)))
   (play-alert-sound
    (poe-item-filter-play-alert-sound-configuration
     (id 2)
     (volume 300)))
   (play-effect
    (poe-item-filter-play-effect-configuration
     (colour 'Grey)))))

(define poe-filter-memory-strands
  (poe-item-filter-block-configuration
   (commentary "Highlight items with memory strands.")
   (memory-strands
    (poe-item-filter-conditional-value-configuration
     (value 1)
     (operator '>=)))
   (set-text-color
    (poe-item-filter-color-configuration
     (red 30) (green 190) (blue 190) (alpha 255)))
   (set-border-color
    (poe-item-filter-color-configuration
     (red 30) (green 190) (blue 190) (alpha 255)))
   (minimap-icon
    (poe-item-filter-minimap-icon-configuration
     (enabled? #t)
     (size 1)
     (colour 'White)
     (shape 'Triangle)))
   (play-alert-sound
    (poe-item-filter-play-alert-sound-configuration
     (id 2)
     (volume 300)))
   (play-effect
    (poe-item-filter-play-effect-configuration
     (colour 'Grey)))))

(define poe-filter-scrolls
  (poe-item-filter-block-configuration
   (commentary "Stop apply rules to scrolls.")
   (base-types '("Portal Scroll"
                 "Scroll of Wisdom"))))

(define poe-filter-vendor-5-linked-sockets
  (poe-item-filter-block-configuration
   (commentary "Highlight 5 linked sockets.")
   (area-level (poe-item-filter-conditional-value-configuration
                (value 75)
                (operator '<)))
   (linked-sockets (poe-item-filter-conditional-value-configuration
                    (value 5)
                    (operator '>=)))
   (set-text-color
    (poe-item-filter-color-configuration
     (red 255) (green 0) (blue 0) (alpha 255)))
   (set-border-color
    (poe-item-filter-color-configuration
     (red 255) (green 0) (blue 0) (alpha 255)))
   (set-background-color
    (poe-item-filter-color-configuration
     (red 255) (green 255) (blue 255) (alpha 255)))
   (play-alert-sound
    (poe-item-filter-play-alert-sound-configuration
     (id 6)
     (volume 300)))
   (play-effect
    (poe-item-filter-play-effect-configuration
     (colour 'Red)))
   (minimap-icon
    (poe-item-filter-minimap-icon-configuration
     (enabled? #t)
     (size 0)
     (colour 'Red)
     (shape 'Star)))))

(define poe-filter-vendor-3-sockets
  (poe-item-filter-block-configuration
   (commentary "Highlight sockets vendor recipe.")
   (area-level (poe-item-filter-conditional-value-configuration
                (value 75)
                (operator '<)))
   (socket-group (poe-item-filter-conditional-value-configuration
                  (value '3RGB)
                  (operator '>=)))
   (set-text-color
    (poe-item-filter-color-configuration
     (red 30) (green 190) (blue 190) (alpha 255)))
   (set-border-color
    (poe-item-filter-color-configuration
     (red 30) (green 190) (blue 190) (alpha 255)))
   (minimap-icon
    (poe-item-filter-minimap-icon-configuration
     (enabled? #t)
     (size 1)
     (colour 'White)
     (shape 'Triangle)))
   (play-alert-sound
    (poe-item-filter-play-alert-sound-configuration
     (id 2)
     (volume 300)))
   (play-effect
    (poe-item-filter-play-effect-configuration
     (colour 'Grey)))))

(define poe-filter-vendor-3-green-sockets
  (poe-item-filter-block-configuration
   (commentary "Highlight 3 green sockets items.")
   (area-level (poe-item-filter-conditional-value-configuration
                (value 68)
                (operator '<)))
   (socket-group (poe-item-filter-conditional-value-configuration
                  (value '3GGG)
                  (operator '>=)))
   (set-text-color
    (poe-item-filter-color-configuration
     (red 30) (green 190) (blue 190) (alpha 255)))
   (set-border-color
    (poe-item-filter-color-configuration
     (red 30) (green 190) (blue 190) (alpha 255)))
   (minimap-icon
    (poe-item-filter-minimap-icon-configuration
     (enabled? #t)
     (size 1)
     (colour 'White)
     (shape 'Triangle)))
   (play-alert-sound
    (poe-item-filter-play-alert-sound-configuration
     (id 2)
     (volume 300)))
   (play-effect
    (poe-item-filter-play-effect-configuration
     (colour 'Grey)))))

(define poe-filter-vendor-6-sockets
  (poe-item-filter-block-configuration
   (commentary "Highlight 6 sockets vendor recipe.")
   (sockets (poe-item-filter-conditional-value-configuration
             (value 6)
             (operator '=)))
   (set-text-color
    (poe-item-filter-color-configuration
     (red 30) (green 190) (blue 190) (alpha 255)))
   (set-border-color
    (poe-item-filter-color-configuration
     (red 30) (green 190) (blue 190) (alpha 255)))
   (minimap-icon
    (poe-item-filter-minimap-icon-configuration
     (enabled? #t)
     (size 1)
     (colour 'White)
     (shape 'Triangle)))
   (play-alert-sound
    (poe-item-filter-play-alert-sound-configuration
     (id 2)
     (volume 300)))
   (play-effect
    (poe-item-filter-play-effect-configuration
     (colour 'Grey)))))

(define poe-filter-unique
  (poe-item-filter-block-configuration
   (commentary "Highlight unique items.")
   (rarity '(Unique))
   (set-text-color
    (poe-item-filter-color-configuration
     (red 188) (green 96) (blue 37) (alpha 255)))
   (set-border-color
    (poe-item-filter-color-configuration
     (red 188) (green 96) (blue 37) (alpha 255)))
   (set-background-color
    (poe-item-filter-color-configuration
     (red 53) (green 13) (blue 13) (alpha 229)))
   (minimap-icon
    (poe-item-filter-minimap-icon-configuration
     (enabled? #t)
     (size 1)
     (colour 'Brown)
     (shape 'Star)))
   (play-alert-sound
    (poe-item-filter-play-alert-sound-configuration
     (id 3)
     (volume 300)))
   (play-effect
    (poe-item-filter-play-effect-configuration
     (colour 'Brown)))))

(define poe-filter-base-high-percentage
  (poe-item-filter-block-configuration
   (commentary "Highlight good percentage.")
   (classes '("Body Armours" "Helmets" "Gloves" "Boots" "Shields"))
   (base-defence-percentile (poe-item-filter-conditional-value-configuration
                             (value 70)
                             (operator '>=)))
   (set-border-color (poe-item-filter-color-configuration
                      (red 0)
                      (green 255)
                      (blue 0)
                      (alpha 255)))
   (continue? #t)))

(define poe-filter-currency-best
  (poe-item-filter-block-configuration
   (commentary "Highlight high value currency.")
   (base-types '("Albino Rhoa Feather"
                 "Awakener's Orb"
                 "Blessing of Chayula"
                 "Blessing of Xoph"
                 "Crusader's Exalted Orb"
                 "Divine Orb"
                 "Eternal Orb"
                 "Fracturing Orb"
                 "Hinekora's Lock"
                 "Hunter's Exalted Orb"
                 "Mirror of Kalandra"
                 "Mirror Shard"
                 "Orb of Dominance"
                 "Redeemer's Exalted Orb"
                 "Reflecting Mist"
                 "Tainted Divine Teardrop"
                 "Valdo's Puzzle Box"
                 "Warlord's Exalted Orb"))
   (set-text-color
    (poe-item-filter-color-configuration
     (red 255) (green 0) (blue 0) (alpha 255)))
   (set-border-color
    (poe-item-filter-color-configuration
     (red 255) (green 0) (blue 0) (alpha 255)))
   (set-background-color
    (poe-item-filter-color-configuration
     (red 255) (green 255) (blue 255) (alpha 255)))
   (play-alert-sound
    (poe-item-filter-play-alert-sound-configuration
     (id 6)
     (volume 300)))
   (play-effect
    (poe-item-filter-play-effect-configuration
     (colour 'Red)))
   (minimap-icon
    (poe-item-filter-minimap-icon-configuration
     (enabled? #t)
     (size 0)
     (colour 'Red)
     (shape 'Star)))))

(define poe-filter-currency-middle
  (poe-item-filter-block-configuration
   (commentary "Highlight middle value currency.")
   (base-types '("Abrasive Catalyst"
                 "Accelerating Catalyst"
                 "Blighted Scouting Report"
                 "Burial Medallion"
                 "Cartographer's Chisel"
                 "Chaos Orb"
                 "Comprehensive Scouting Report"
                 "Delirious Scouting Report"
                 "Engineer's Orb"
                 "Exalted Shard"
                 "Gemcutter's Prism"
                 "Glassblower's Bauble"
                 "Grand Eldritch Ember"
                 "Grand Eldritch Ichor"
                 "Greater Eldritch Ember"
                 "Greater Eldritch Ichor"
                 "Harbinger's Orb"
                 "Imbued Catalyst"
                 "Intrinsic Catalyst"
                 "Lesser Eldritch Ember"
                 "Lesser Eldritch Ichor"
                 "Maven's Chisel of Avarice"
                 "Maven's Chisel of Divination"
                 "Maven's Chisel of Procurement"
                 "Noxious Catalyst"
                 "Operative's Scouting Report"
                 "Orb of Unmaking"
                 "Primal Crystallised Lifeforce"
                 "Ritual Splinter"
                 "Scrap Metal"
                 "Singular Scouting Report"
                 "Stacked Deck"
                 "Tainted Armourer's Scrap"
                 "Tainted Blacksmith's Whetstone"
                 "Tainted Jeweller's Orb"
                 "Tempering Catalyst"
                 "Turbulent Catalyst"
                 "Unstable Catalyst"
                 "Veiled Scarab"
                 "Vivid Crystallised Lifeforce"
                 "Wild Crystallised Lifeforce"))
   (set-text-color
    (poe-item-filter-color-configuration
     (red 0) (green 0) (blue 0) (alpha 255)))
   (set-border-color
    (poe-item-filter-color-configuration
     (red 0) (green 0) (blue 0) (alpha 255)))
   (set-background-color
    (poe-item-filter-color-configuration
     (red 249) (green 150) (blue 25) (alpha 255)))
   (play-alert-sound
    (poe-item-filter-play-alert-sound-configuration
     (id 2)
     (volume 300)))
   (play-effect
    (poe-item-filter-play-effect-configuration
     (colour 'White)))
   (minimap-icon
    (poe-item-filter-minimap-icon-configuration
     (enabled? #t)
     (size 2)
     (colour 'White)
     (shape 'Circle)))))

(define poe-filter-fragments
  (poe-item-filter-block-configuration
   (commentary "Highlight map fragments.")
   (base-types '("Divine Vessel"
                 "Sacrifice at Dawn"
                 "Sacrifice at Dusk"
                 "Sacrifice at Midnight"
                 "Sacrifice at Noon"
                 "Offering To The Goddess"
                 "Tribute To The Goddess"))
   (set-text-color
    (poe-item-filter-color-configuration
     (red 178) (green 120) (blue 230) (alpha 240)))
   (set-border-color
    (poe-item-filter-color-configuration
     (red 175) (green 120) (blue 230) (alpha 240)))
   (minimap-icon
    (poe-item-filter-minimap-icon-configuration
     (enabled? #t)
     (size 0)
     (colour 'Red)
     (shape 'Square)))
   (play-effect
    (poe-item-filter-play-effect-configuration
     (colour 'Yellow)))))

(define poe-filter-wombgifts
  (poe-item-filter-block-configuration
   (commentary "Highlight wombgifts.")
   (classes '("Wombgifts"))
   (set-text-color
    (poe-item-filter-color-configuration
     (red 0) (green 0) (blue 0) (alpha 255)))
   (set-border-color
    (poe-item-filter-color-configuration
     (red 0) (green 0) (blue 0) (alpha 255)))
   (set-background-color
    (poe-item-filter-color-configuration
     (red 120) (green 200) (blue 160) (alpha 255)))
   (play-alert-sound
    (poe-item-filter-play-alert-sound-configuration
     (id 2)
     (volume 300)))
   (play-effect
    (poe-item-filter-play-effect-configuration
     (colour 'Yellow)))
   (minimap-icon
    (poe-item-filter-minimap-icon-configuration
     (enabled? #t)
     (size 0)
     (colour 'Yellow)
     (shape 'Moon)))))

(define poe-filter-idols
  (poe-item-filter-block-configuration
   (commentary "Highlight Idols.")
   (classes '("Idols"))
   (set-text-color
    (poe-item-filter-color-configuration
     (red 255) (green 178) (blue 135) (alpha 255)))
   (set-border-color
    (poe-item-filter-color-configuration
     (red 255) (green 178) (blue 135) (alpha 255)))
   (set-background-color
    (poe-item-filter-color-configuration
     (red 20) (green 20) (blue 0) (alpha 255)))
   (play-effect
    (poe-item-filter-play-effect-configuration
     (colour 'Orange)))))

(define poe-filter-currency+incubators
  (poe-item-filter-block-configuration
   (commentary "Highlight currency and incubators.")
   (classes '("Currency" "Incubators"))
   (set-text-color
    (poe-item-filter-color-configuration
     (red 0) (green 0) (blue 0) (alpha 255)))
   (set-border-color
    (poe-item-filter-color-configuration
     (red 0) (green 0) (blue 0) (alpha 255)))
   (set-background-color
    (poe-item-filter-color-configuration
     (red 213) (green 159) (blue 0) (alpha 255)))
   (minimap-icon
    (poe-item-filter-minimap-icon-configuration
     (enabled? #t)
     (size 2)
     (colour 'White)
     (shape 'Circle)))
   (play-effect
    (poe-item-filter-play-effect-configuration
     (colour 'White)))))

(define poe-filter-jewelry
  (poe-item-filter-block-configuration
   (commentary "Highlight jewelry.")
   (classes '("Amulet" "Belt" "Ring"))
   (set-border-color
    (poe-item-filter-color-configuration
     (red 0) (green 0) (blue 0) (alpha 255)))
   (set-background-color
    (poe-item-filter-color-configuration
     (red 0) (green 100) (blue 150) (alpha 255)))
   (continue? #t)))

(define poe-filter-jewelry-best
  (poe-item-filter-block-configuration
   (commentary "Highlight high value jewelry.")
   (base-types (append '("Amethyst Ring"
                         "Astrolabe Amulet"
                         "Blue Pearl Amulet"
                         "Focused Amulet"
                         "Gold Amulet"
                         "Gold Ring"
                         "Jet Amulet"
                         "Marble Amulet"
                         "Onyx Amulet"
                         "Ruby Amulet"
                         "Seaglass Amulet"
                         "Simplex Amulet"
                         "Unset Amulet")
                       '("Fugitive Ring")))
   (set-border-color
    (poe-item-filter-color-configuration
     (red 0) (green 0) (blue 0) (alpha 255)))
   (set-background-color
    (poe-item-filter-color-configuration
     (red 38) (green 0) (blue 86) (alpha 255)))
   (set-font-size 45)
   (continue? #t)))

(define poe-filter-talismans
  (poe-item-filter-block-configuration
   (commentary "Highlight talismans.")
   (base-types '("Talisman"))
   (set-border-color
    (poe-item-filter-color-configuration
     (red 0) (green 0) (blue 0) (alpha 255)))
   (set-background-color
    (poe-item-filter-color-configuration
     (red 38) (green 0) (blue 86) (alpha 255)))
   (set-font-size 45)
   (continue? #t)))

(define poe-filter-belts
  (poe-item-filter-block-configuration
   (commentary "Highlight cloth belt.")
   (base-types '("Cloth Belt"))
   (set-font-size 45)
   (continue? #t)))

(define poe-filter-belts-best
  (poe-item-filter-block-configuration
   (commentary "Highlight high value belts.")
   (base-types '("Stygian Vise" "Vanguard Belt"))
   (play-effect
    (poe-item-filter-play-effect-configuration
     (colour 'Green)))
   (set-font-size 45)
   (continue? #t)))

(define poe-filter-labyrinth+incursion
  (poe-item-filter-block-configuration
   (commentary "Highlight labyrinth and incursion items.")
   (classes '("Incursion Items" "Labyrinth Items"))
   (set-text-color
    (poe-item-filter-color-configuration
     (red 74) (green 230) (blue 58) (alpha 255)))
   (set-border-color
    (poe-item-filter-color-configuration
     (red 74) (green 230) (blue 58) (alpha 255)))
   (set-background-color
    (poe-item-filter-color-configuration
     (red 20) (green 20) (blue 0) (alpha 255)))
   (play-alert-sound
    (poe-item-filter-play-alert-sound-configuration
     (id 3)
     (volume 300)))
   (play-effect
    (poe-item-filter-play-effect-configuration
     (colour 'Green)))
   (minimap-icon
    (poe-item-filter-minimap-icon-configuration
     (enabled? #t)
     (size 0)
     (colour 'Green)
     (shape 'Pentagon)))))

(define poe-filter-scarabs
  (poe-item-filter-block-configuration
   (commentary "Highlight scarabs.")
   (base-types '("Scarab"))
   (set-text-color
    (poe-item-filter-color-configuration
     (red 255) (green 0) (blue 255) (alpha 255)))
   (set-border-color
    (poe-item-filter-color-configuration
     (red 255) (green 0) (blue 255) (alpha 255)))
   (set-background-color
    (poe-item-filter-color-configuration
     (red 100)
     (green 0)
     (blue 100)
     (alpha 255)))
   (play-alert-sound
    (poe-item-filter-play-alert-sound-configuration
     (id 3)
     (volume 300)))
   (play-effect
    (poe-item-filter-play-effect-configuration
     (colour 'Pink)))
   (minimap-icon
    (poe-item-filter-minimap-icon-configuration
     (enabled? #t)
     (size 0)
     (colour 'Pink)
     (shape 'Circle)))))

(define poe-filter-maps
  (poe-item-filter-block-configuration
   (commentary "Highlight maps.")
   (classes '("Maps"
              "Map Fragments"
              "Expedition Logbook"))
   (set-text-color
    (poe-item-filter-color-configuration
     (red 0) (green 0) (blue 0) (alpha 255)))
   (set-background-color
    (poe-item-filter-color-configuration
     (red 235) (green 235) (blue 235) (alpha 255)))
   (play-alert-sound
    (poe-item-filter-play-alert-sound-configuration
     (id 5)
     (volume 300)))
   (play-effect
    (poe-item-filter-play-effect-configuration
     (colour 'Yellow)))
   (minimap-icon
    (poe-item-filter-minimap-icon-configuration
     (enabled? #t)
     (size 0)
     (colour 'Red)
     (shape 'Square)))))

(define %poe-filter-jewel
  (poe-item-filter-block-configuration
   (set-text-color
    (poe-item-filter-color-configuration
     (red 0) (green 240) (blue 190) (alpha 255)))
   (set-border-color
    (poe-item-filter-color-configuration
     (red 0) (green 240) (blue 190) (alpha 255)))
   (set-background-color
    (poe-item-filter-color-configuration
     (red 47)
     (green 0)
     (blue 74)
     (alpha 255)))
   (play-effect
    (poe-item-filter-play-effect-configuration
     (colour 'Purple)))
   (minimap-icon
    (poe-item-filter-minimap-icon-configuration
     (enabled? #t)
     (size 1)
     (colour 'Purple)
     (shape 'Diamond)))))

(define poe-filter-jewels
  (poe-item-filter-block-configuration
   (inherit %poe-filter-jewel)
   (commentary "Highlight jewels.")
   (base-types (append '("Cobalt Jewel"
                         "Crimson Jewel"
                         "Viridian Jewel")
                       '("Small Cluster Jewel"
                         "Medium Cluster Jewel"
                         "Large Cluster Jewel")))))

(define poe-filter-abyss-jewels
  (poe-item-filter-block-configuration
   (inherit %poe-filter-jewel)
   (commentary "Highlight abyss jewels.")
   (classes '("Abyss Jewels"))))

(define %poe-filter-gem
  (poe-item-filter-block-configuration
   (set-text-color
    (poe-item-filter-color-configuration
     (red 30) (green 190) (blue 190) (alpha 255)))
   (set-border-color
    (poe-item-filter-color-configuration
     (red 30) (green 190) (blue 190) (alpha 255)))
   (classes '("Skill Gems" "Support Gems"))
   (minimap-icon
    (poe-item-filter-minimap-icon-configuration
     (enabled? #t)
     (size 1)
     (colour 'White)
     (shape 'Triangle)))
   (play-alert-sound
    (poe-item-filter-play-alert-sound-configuration
     (id 2)
     (volume 300)))
   (play-effect
    (poe-item-filter-play-effect-configuration
     (colour 'Grey)))))

(define %poe-filter-high-value-gem
  (poe-item-filter-block-configuration
   (classes '("Skill Gems"
              "Support Gems"))
   (set-text-color
    (poe-item-filter-color-configuration
     (red 255) (green 0) (blue 0) (alpha 255)))
   (set-border-color
    (poe-item-filter-color-configuration
     (red 255) (green 0) (blue 0) (alpha 255)))
   (set-background-color
    (poe-item-filter-color-configuration
     (red 255) (green 255) (blue 255) (alpha 255)))
   (play-alert-sound
    (poe-item-filter-play-alert-sound-configuration
     (id 6)
     (volume 300)))
   (play-effect
    (poe-item-filter-play-effect-configuration
     (colour 'Red)))
   (minimap-icon
    (poe-item-filter-minimap-icon-configuration
     (enabled? #t)
     (size 0)
     (colour 'Red)
     (shape 'Star)))))

(define poe-filter-level-gems
  (poe-item-filter-block-configuration
   (inherit %poe-filter-gem)
   (commentary "Highlight level gems.")))

(define poe-filter-high-level-gems
  (poe-item-filter-block-configuration
   (inherit %poe-filter-gem)
   (commentary "Highlight high level gems.")
   (gem-level (poe-item-filter-conditional-value-configuration
               (value 20)
               (operator '>=)))))

(define poe-filter-high-quality-gems
  (poe-item-filter-block-configuration
   (inherit %poe-filter-gem)
   (commentary "Highlight high quality gems.")
   (quality (poe-item-filter-conditional-value-configuration
             (value 1)
             (operator '>=)))))

(define poe-filter-high-value-gems
  (poe-item-filter-block-configuration
   (inherit %poe-filter-high-value-gem)
   (commentary "Highlight specific awakend gems.")
   (base-types '("Awakened Ancestral Call Support"
                 "Awakened Brutality Support"
                 "Awakened Cast On Critical Strike Support"
                 "Awakened Chain Support"
                 "Awakened Elemental Damage with Attacks Support"
                 "Awakened Empower Support"
                 "Awakened Enhance Support"
                 "Awakened Enlighten Support"
                 "Awakened Fork Support"
                 "Awakened Generosity Support"
                 "Awakened Greater Multiple Projectiles Support"
                 "Awakened Increased Area of Effect Support"
                 "Awakened Melee Physical Damage Support"
                 "Awakened Multistrike Support"
                 "Awakened Spell Cascade Support"
                 "Awakened Spell Echo Support"
                 "Awakened Swift Affliction Support"
                 "Awakened Unbound Ailments Support"
                 "Awakened Unleash Support"
                 "Awakened Void Manipulation Support"))))

(define poe-filter-awakend-gems
  (poe-item-filter-block-configuration
   (inherit %poe-filter-high-value-gem)
   (commentary "Highlight awakend gems.")
   (base-types '("Awakened"))))

(define poe-filter-transfigured-gems
  (poe-item-filter-block-configuration
   (inherit %poe-filter-high-value-gem)
   (commentary "Highlight transfigured gems.")
   (transfigured-gem? #t)))

(define poe-filter-rogue-marks
  (poe-item-filter-block-configuration
   (commentary "Highlight Rogue markers.")
   (base-types '("Rogue's Marker"))
   (set-text-color
    (poe-item-filter-color-configuration
     (red 255) (green 178) (blue 135) (alpha 255)))
   (set-border-color
    (poe-item-filter-color-configuration
     (red 255) (green 178) (blue 135) (alpha 255)))
   (set-background-color
    (poe-item-filter-color-configuration
     (red 20) (green 20) (blue 0) (alpha 255)))
   (play-effect
    (poe-item-filter-play-effect-configuration
     (colour 'Orange)))))

(define poe-filter-blueprints+contracts+sanctum
  (poe-item-filter-block-configuration
   (commentary "Highlight Blueprints and Contracts and Sanctum.")
   (classes '("Blueprints" "Contracts" "Sanctum Research"))
   (set-text-color
    (poe-item-filter-color-configuration
     (red 255) (green 85) (blue 85) (alpha 255)))
   (set-border-color
    (poe-item-filter-color-configuration
     (red 255) (green 85) (blue 85) (alpha 255)))
   (set-background-color
    (poe-item-filter-color-configuration
     (red 40) (green 0) (blue 30) (alpha 255)))
   (play-effect
    (poe-item-filter-play-effect-configuration
     (colour 'Yellow)))
   (play-alert-sound
    (poe-item-filter-play-alert-sound-configuration
     (id 5)
     (volume 300)))
   (minimap-icon
    (poe-item-filter-minimap-icon-configuration
     (enabled? #t)
     (size 1)
     (colour 'Yellow)
     (shape 'UpsideDownHouse)))))

(define poe-filter-not-identified-items
  (poe-item-filter-block-configuration
   (commentary "Highlight not identified items.")
   (identified? #f)
   (rarity '(Magic Rare Unique))
   (set-background-color
    (poe-item-filter-color-configuration
     (red 86) (green 0) (blue 0) (alpha 230)))
   (continue? #t)))

(define base-items
  (json-string->scm
   (with-input-from-file "/home/oleg/src/cgit.wugi.info/wigust/dotfiles/src/guile/path-of-building-data-json/input.json"
     read-string)))

(define poe-filters-weak-bases
  (map (lambda (type)
         (poe-item-filter-block-configuration
          (commentary (format #f "Lower ~s base items for low tier types."
                              type type))
          (rarity '(Normal Magic Rare))
          (base-types (sort (map (lambda (item)
                                   (first item))
                                 (filter (lambda (item)
                                           (and (and=> (assoc-ref item "type")
                                                       (lambda (sub-type)
                                                         (and (string= sub-type type)
                                                              (and=> (assoc-ref item "type")
                                                                     (lambda (t)
                                                                       (string= type t))))))
                                                (and=> (assoc-ref item "req")
                                                       (lambda (req)
                                                         (and=> (assoc-ref req "level")
                                                                (lambda (level)
                                                                  (< level 69)))))))
                                         base-items))
                            string<))
          (set-font-size 20)
          (area-level (poe-item-filter-conditional-value-configuration
                       (value 75)
                       (operator '>)))
          (continue? #t)))
       '("Body Armour"
         "Boots"
         "Gloves"
         "Helmet")))

(define* (poe-filters-unused-bases bases #:key ruthless?)
  (delete #f
          (apply append
                 (map (lambda (type)
                        (map (lambda (base-type)
                               (if (and (string= base-type "Energy Shield")
                                        (string= type "Body Armour"))
                                   #f
                                   (let ((filter-definition
                                          (poe-item-filter-block-configuration
                                           (commentary (format #f "Hide ~s ~s base items for specific defence types."
                                                               type base-type))
                                           (rarity '(Normal Magic Rare))
                                           (base-types
                                            (sort (let ((items
                                                         (filter (lambda (item)
                                                                   (and=> (assoc-ref item "subType")
                                                                          (lambda (sub-type)
                                                                            (and (string= sub-type base-type)
                                                                                 (and=> (assoc-ref item "type")
                                                                                        (lambda (t)
                                                                                          (string= type t)))))))
                                                                 base-items)))
                                                    (map (lambda (item)
                                                           (string-replace-substring (first item)
                                                                                     (format #f " (~a)" base-type)
                                                                                     ""))
                                                         items))
                                                  string<))
                                           (set-font-size 20)
                                           (show? #f)
                                           (continue? #t))))
                                     (if ruthless?
                                         filter-definition
                                         (poe-item-filter-block-configuration
                                          (inherit filter-definition)
                                          (set-background-color
                                           (poe-item-filter-color-configuration
                                            (red 0)
                                            (green 0)
                                            (blue 0)
                                            (alpha 0))))))))
                             bases))
                      '("Body Armour"
                        "Boots"
                        "Gloves"
                        "Helmet")))))

(define (poe-filters-best-bases include-sub-types)
  (delete #f
          (map (lambda (base-type)
                 (poe-item-filter-block-configuration
                  (commentary (format #f "Increase font size for high level ~s base items."
                                      base-type))
                  (base-types
                   (sort (let ((items
                                (filter (lambda (item)
                                          (and=> (assoc-ref item "req")
                                                 (lambda (req)
                                                   (and=> (assoc-ref req "level")
                                                          (lambda (level)
                                                            (>= level 79))))))
                                        base-items)))
                           (map (lambda (item)
                                  (string-replace-substring (first item)
                                                            (format #f " (~a)" base-type)
                                                            ""))
                                items))
                         string<))
                  (set-font-size 45)
                  (continue? #t)))
               include-sub-types)))

(define poe-filter-best-sceptres
  (poe-item-filter-block-configuration
   (commentary "Increase font size for high level sceptres base items.")
   (base-types '("Opal Sceptre" "Void Sceptre"))
   (set-font-size 45)
   (continue? #t)))

(define poe-filter-best-wands
  (poe-item-filter-block-configuration
   (commentary "Increase font size for high level wands base items.")
   (base-types '("Profane Wand"))
   (set-font-size 45)
   (continue? #t)))

(define poe-filter-best-staffs
  (poe-item-filter-block-configuration
   (commentary "Increase font size for high level wands base items.")
   (base-types '("Moon Staff"))
   (set-font-size 45)
   (continue? #t)))

(define* (poe-filter-unused-weapons bases #:key ruthless?)
  (let ((filter-definition
         (poe-item-filter-block-configuration
          (commentary "Decrease font size for items with classes.")
          (classes bases)
          (set-font-size 20)
          (show? #f)
          (continue? #t))))
    (if ruthless?
        filter-definition
        (poe-item-filter-block-configuration
         (inherit filter-definition)
         (set-background-color
          (poe-item-filter-color-configuration
           (red 0)
           (green 0)
           (blue 0)
           (alpha 0)))))))

(define poe-filter-flasks
  (poe-item-filter-block-configuration
   (commentary "Highlight flasks.")
   (classes '("Life Flasks" "Mana Flasks" "Hybrid Flasks"))
   (set-border-color
    (poe-item-filter-color-configuration
     (red 0)
     (green 0)
     (blue 0)
     (alpha 255)))
   (set-background-color
    (poe-item-filter-color-configuration
     (red 25)
     (green 100)
     (blue 75)
     (alpha 255)))
   (continue? #t)))

(define %poe-filter-flasks-color
  (poe-item-filter-color-configuration
   (red 140)
   (green 60)
   (blue 25)
   (alpha 255)))

(define poe-filter-utility-flasks
  (poe-item-filter-block-configuration
   (commentary "Highlight utility flasks.")
   (classes '("Utility Flasks"))
   (set-border-color %poe-filter-flasks-color)
   (set-background-color %poe-filter-flasks-color)
   (continue? #t)))

(define poe-filter-tinctures
  (poe-item-filter-block-configuration
   (commentary "Highlight tinctures.")
   (classes '("Tinctures"))
   (set-border-color %poe-filter-flasks-color)
   (set-background-color %poe-filter-flasks-color)))

(define poe-filter-best-hybrid-flasks
  (poe-item-filter-block-configuration
   (commentary "Highlight hybrid flasks.")
   (base-types '("Hallowed Hybrid Flask"))
   (set-border-color %poe-filter-flasks-color)
   (set-background-color %poe-filter-flasks-color)))

(define poe-filter-low-level-flasks
  (poe-item-filter-block-configuration
   (classes '("Life Flasks" "Mana Flasks" "Hybrid Flasks"))
   (commentary "Highlight flasks.")
   (rarity '(Normal))
   (show? #f)
   (item-level (poe-item-filter-conditional-value-configuration
                (value 83)
                (operator '<=)))
   (continue? #t)))

(define poe-filter-high-quality-flasks
  (poe-item-filter-block-configuration
   (commentary "Highlight high quality flasks.")
   (classes '("Life Flasks" "Mana Flasks" "Hybrid Flasks" "Utility Flasks"))
   (quality (poe-item-filter-conditional-value-configuration
             (value 1)
             (operator '>=)))
   (play-effect
    (poe-item-filter-play-effect-configuration
     (colour 'Green)))))

(define poe-filter-fractured-items
  (poe-item-filter-block-configuration
   (commentary "Show fractured items.")
   (fractured-item? #t)
   (show? #t)
   (continue? #t)))

(define poe-filter-identified-items
  (poe-item-filter-block-configuration
   (commentary "Hide identified items.")
   (rarity '(Magic Rare Unique))
   (identified? #t)
   (show? #f)))

(define (main args)
  (define opts
    (parse-command-line args %options (list '())))
  (define exclude-sub-types
    (filter-map (match-lambda
                  (('exclude-defence . type) type)
                  (_ #f))
                opts))
  (define include-sub-types
    (filter-map (match-lambda
                  (('defence . type) type)
                  (_ #f))
                opts))
  (define exclude-weapons
    (filter-map (match-lambda
                  (('exclude-weapon . type) type)
                  (_ #f))
                opts))
  (define ruthless?
    (assoc-ref opts 'ruthless?))
  (define output
    (assoc-ref opts 'output))
  (define poe-filter-blocks
    (append (if ruthless? (list poe-filter-level-gems) '())
            (list poe-filter-basic
                  poe-filter-crafting
                  poe-filter-quality-low-level
                  poe-filter-quality
                  poe-filter-memory-strands
                  poe-filter-scrolls
                  poe-filter-vendor-5-linked-sockets
                  poe-filter-vendor-3-sockets
                  poe-filter-vendor-3-green-sockets
                  poe-filter-vendor-6-sockets
                  poe-filter-unique
                  poe-filter-currency-best
                  poe-filter-currency-middle
                  poe-filter-fragments
                  poe-filter-wombgifts
                  poe-filter-idols
                  poe-filter-currency+incubators
                  (if ruthless?
                      (poe-item-filter-block-configuration
                       (inherit poe-filter-jewelry)
                       (continue? #f))
                      poe-filter-jewelry)
                  poe-filter-jewelry-best
                  poe-filter-talismans
                  poe-filter-belts
                  poe-filter-belts-best
                  poe-filter-labyrinth+incursion
                  poe-filter-scarabs
                  poe-filter-maps
                  poe-filter-jewels
                  poe-filter-abyss-jewels
                  poe-filter-high-level-gems
                  poe-filter-high-quality-gems
                  poe-filter-high-value-gems
                  poe-filter-awakend-gems
                  poe-filter-transfigured-gems
                  poe-filter-rogue-marks
                  poe-filter-blueprints+contracts+sanctum
                  poe-filter-not-identified-items)

            poe-filters-weak-bases

            (if (null? exclude-sub-types)
                '()
                (poe-filters-unused-bases exclude-sub-types
                                          #:ruthless? ruthless?))

            (if (null? include-sub-types)
                '()
                (poe-filters-best-bases include-sub-types))

            (list poe-filter-best-sceptres
                  poe-filter-best-wands
                  poe-filter-best-staffs)

            (if (null? exclude-weapons)
                '()
                (list (poe-filter-unused-weapons exclude-weapons
                                                 #:ruthless? ruthless?)))
            (list poe-filter-flasks
                  poe-filter-utility-flasks
                  poe-filter-tinctures
                  poe-filter-best-hybrid-flasks
                  poe-filter-low-level-flasks
                  poe-filter-high-quality-flasks
                  poe-filter-fractured-items
                  poe-filter-identified-items)))
  (let* ((%store (open-connection))
         (drv
          (run-with-store %store
            (text-file* "wigust.filter"
                        (serialize-configuration
                         (poe-item-filter-configuration
                          (blocks (if ruthless?
                                      (map (lambda (block)
                                             (poe-item-filter-block-configuration
                                              (inherit block)
                                              (show? #t)))
                                           poe-filter-blocks)
                                      poe-filter-blocks)))
                         poe-item-filter-configuration-fields)))))
    (and (build-derivations %store (list drv))
         (let ((out (derivation->output-path drv)))
           (display out)
           (newline)
           (copy-file out "wigust.filter")
           (copy-file out output)))))
