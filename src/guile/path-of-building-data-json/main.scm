;; guix build -f main.scm

(use-modules (ice-9 format)
             (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 string-fun)

             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-171)

             (json)

             (gnu services configuration)
             (guix gexp)
             (guix store))

(define (uglify-field-name field-name)
  (apply string-append
         (map string-titlecase
              (string-split (string-delete #\?
                                           (symbol->string field-name))
                            #\-))))

(define show?
  boolean?)

(define (serialize-show field value)
  #~(format #f "~a~%" (if #$value "Show" "Hide")))

(define (serialize-string field-name value)
  #~(format #f "~a ~a~%" #$field-name #$value))

(define (serialize-list-of-strings field-name value)
  #~(string-append #$@(map (cut serialize-string field-name <>) #$value)))

(define-maybe list-of-strings)

(define (serialize-symbol-1 field-name value)
  #~(symbol->string '#$value))

(define (serialize-list-of-symbols field-name value)
  #~(if (null? '#$value)
        ""
        (string-append "\t"
                       (string-join (append (list #$(uglify-field-name field-name))
                                            (map (lambda (v)
                                                   (symbol->string v))
                                                 '#$value)))
                       "\n")))

(define-maybe list-of-symbols)

(define set-font-size? integer?)

(define (serialize-set-font-size field-name value)
  #~(if (= #$value 0)
        ""
        (format #f "\t~a ~a~%" #$(uglify-field-name field-name) #$value)))

(define base-types?
  list-of-strings?)

(define (serialize-base-types field-name value)
  #~(if (null? '#$value)
        ""
        (if (> (length '#$value) 1)
            (format #f "\tBaseType == ~{~s ~}~%" '#$value)
            (format #f "\tBaseType ~{~s ~}~%" '#$value))))

(define classes?
  list-of-strings?)

(define (serialize-classes field-name value)
  #~(if (null? '#$value)
        ""
        (format #f "\tClass == ~{~s ~}~%"
                '#$value)))

(define commentary? string?)

(define (serialize-commentary field-name value)
  #~(if (string-null? #$value)
        ""
        (format #f "# ~a~%" #$value)))

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
  #~(symbol->string '#$value))

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
  #~(string-join
     (list #$@(list-transduce (base-transducer config) rcons fields))))

(define conditional-value? poe-item-filter-conditional-value-configuration?)
(define (serialize-conditional-value field-name value)
  #~(if #$(poe-item-filter-conditional-value-configuration-operator value)
        (format #f
                "\t~a ~a~%"
                #$(uglify-field-name field-name)
                #$(serialize-conditional-value-configuration value poe-item-filter-conditional-value-configuration-fields))
        ""))

(define continue? boolean?)

(define (serialize-continue field-name value)
  #~(if #$value
        (string-append "\t" #$(uglify-field-name field-name) "\n")
        ""))

(define (serialize-integer field-name value)
  #~(#$value))

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
  #~(if (or (and (or (> #$(poe-item-filter-color-configuration-red value) 0)
                     (> #$(poe-item-filter-color-configuration-green value) 0)
                     (> #$(poe-item-filter-color-configuration-blue value) 0)
                     (> #$(poe-item-filter-color-configuration-alpha value) 0))
                 (or (< #$(poe-item-filter-color-configuration-red value) 256)
                     (< #$(poe-item-filter-color-configuration-green value) 256)
                     (< #$(poe-item-filter-color-configuration-blue value) 256)
                     (< #$(poe-item-filter-color-configuration-alpha value) 256)))
            (or (= #$(poe-item-filter-color-configuration-red value) 0)
                (= #$(poe-item-filter-color-configuration-green value) 0)
                (= #$(poe-item-filter-color-configuration-blue value) 0)
                (= #$(poe-item-filter-color-configuration-alpha value) 0)))
        (string-append "\t"
                       (string-join (list #$(uglify-field-name field-name)
                                          #$(number->string (poe-item-filter-color-configuration-red value))
                                          #$(number->string (poe-item-filter-color-configuration-green value))
                                          #$(number->string (poe-item-filter-color-configuration-blue value))
                                          #$(number->string (poe-item-filter-color-configuration-alpha value))))
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
  #~(if #$(poe-item-filter-minimap-icon-configuration-enabled? value)
        (string-append "\t"
                       (string-join (list #$(uglify-field-name field-name)
                                          #$(number->string (poe-item-filter-minimap-icon-configuration-size value))
                                          #$(symbol->string (poe-item-filter-minimap-icon-configuration-colour value))
                                          #$(symbol->string (poe-item-filter-minimap-icon-configuration-shape value))))
                       "\n")
        ""))

(define (boolean-with-field? x)
  (or (and (symbol? x)
           (eq? 'disabled x))
      (boolean? x)))

(define (serialize-boolean-with-field field-name value)
  #~(if (and (symbol? '#$value)
             (eq? 'disabled '#$value))
        ""
        (string-append "\t"
                       (string-join (list #$(uglify-field-name field-name)
                                          #$(if value "True" "False")))
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
  (stack-size
   (conditional-value (poe-item-filter-conditional-value-configuration))
   "")
  (quality
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

(define %weapon-classes
  '("Bows"
    "Claws"
    "Corpses"
    "Daggers"
    "One Hand Axes"
    "One Hand Maces"
    "One Hand Swords"
    "Quivers"
    "Rune Daggers"
    ;; "Sceptres"
    ;; "Staves"
    "Thrusting One Hand Swords"
    "Two Hand Axes"
    "Two Hand Maces"
    "Two Hand Swords"
    ;; "Wands"
    ;; "Warstaves"
    ))

(define (generate-filter)
  (define font-size 20)
  (define base-items
    (json-string->scm
     (with-input-from-file "input.json"
       read-string)))
  (define exclude-sub-types
    '("Armour"
      ;; "Armour/Energy Shield"
      "Armour/Evasion"
      "Energy Shield"
      "Evasion"
      "Evasion/Energy Shield"))
    (define include-sub-types
    '("Armour/Energy Shield"
      "Energy Shield"))
  #~(begin
      (use-modules (ice-9 format))
      #$(serialize-configuration
         (poe-item-filter-configuration
          (blocks
           (append (list (poe-item-filter-block-configuration
                          (commentary "Highlight border of best crafting bases.")
                          (item-level (poe-item-filter-conditional-value-configuration
                                       (value 82)
                                       (operator '>=)))
                          (set-border-color (poe-item-filter-color-configuration
                                             (red 74)
                                             (green 230)
                                             (blue 58)
                                             (alpha 255)))
                          (continue? #t))
                         (poe-item-filter-block-configuration
                          (quality (poe-item-filter-conditional-value-configuration
                                    (value 20)
                                    (operator '>=)))
                          (set-text-color
                           (poe-item-filter-color-configuration
                            (red 30)
                            (green 190)
                            (blue 190)
                            (alpha 255)))
                          (set-border-color
                           (poe-item-filter-color-configuration
                            (red 30)
                            (green 190)
                            (blue 190)
                            (alpha 255)))
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
                            (colour 'Grey))))

                         (poe-item-filter-block-configuration
                          (commentary "Stop apply rules to scrolls.")
                          (base-types '("Portal Scroll"
                                        "Scroll of Wisdom")))

                         (poe-item-filter-block-configuration
                          (commentary "Highlight 5 linked sockets.")
                          (linked-sockets (poe-item-filter-conditional-value-configuration
                                           (value 5)
                                           (operator '>=)))
                          (set-text-color
                           (poe-item-filter-color-configuration
                            (red 255)
                            (green 0)
                            (blue 0)
                            (alpha 255)))
                          (set-border-color
                           (poe-item-filter-color-configuration
                            (red 255)
                            (green 0)
                            (blue 0)
                            (alpha 255)))
                          (set-background-color
                           (poe-item-filter-color-configuration
                            (red 255)
                            (green 255)
                            (blue 255)
                            (alpha 255)))
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
                            (shape 'Star))))

                         ;; (poe-item-filter-block-configuration
                         ;;  (commentary "Highlight sockets vendor recipe.")
                         ;;  (socket-group (poe-item-filter-conditional-value-configuration
                         ;;                 (value '3RGB)
                         ;;                 (operator '>=)))
                         ;;  (set-text-color
                         ;;   (poe-item-filter-color-configuration
                         ;;    (red 30)
                         ;;    (green 190)
                         ;;    (blue 190)
                         ;;    (alpha 255)))
                         ;;  (set-border-color
                         ;;   (poe-item-filter-color-configuration
                         ;;    (red 30)
                         ;;    (green 190)
                         ;;    (blue 190)
                         ;;    (alpha 255)))
                         ;;  (minimap-icon
                         ;;   (poe-item-filter-minimap-icon-configuration
                         ;;    (enabled? #t)
                         ;;    (size 1)
                         ;;    (colour 'White)
                         ;;    (shape 'Triangle)))
                         ;;  (play-alert-sound
                         ;;   (poe-item-filter-play-alert-sound-configuration
                         ;;    (id 2)
                         ;;    (volume 300)))
                         ;;  (play-effect
                         ;;   (poe-item-filter-play-effect-configuration
                         ;;    (colour 'Grey))))

                         (poe-item-filter-block-configuration
                          (commentary "Highlight 6 sockets vendor recipe.")
                          (sockets (poe-item-filter-conditional-value-configuration
                                    (value 6)
                                    (operator '=)))
                          (set-text-color
                           (poe-item-filter-color-configuration
                            (red 30)
                            (green 190)
                            (blue 190)
                            (alpha 255)))
                          (set-border-color
                           (poe-item-filter-color-configuration
                            (red 30)
                            (green 190)
                            (blue 190)
                            (alpha 255)))
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
                            (colour 'Grey))))

                         (poe-item-filter-block-configuration
                          (commentary "Highlight unique items.")
                          (rarity '(Unique))
                          (set-text-color
                           (poe-item-filter-color-configuration
                            (red 188)
                            (green 96)
                            (blue 37)
                            (alpha 255)))
                          (set-border-color
                           (poe-item-filter-color-configuration
                            (red 188)
                            (green 96)
                            (blue 37)
                            (alpha 255)))
                          (set-background-color
                           (poe-item-filter-color-configuration
                            (red 53)
                            (green 13)
                            (blue 13)
                            (alpha 229)))
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
                            (colour 'Brown))))

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
                          (continue? #t))

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
                            (red 255)
                            (green 0)
                            (blue 0)
                            (alpha 255)))
                          (set-border-color
                           (poe-item-filter-color-configuration
                            (red 255)
                            (green 0)
                            (blue 0)
                            (alpha 255)))
                          (set-background-color
                           (poe-item-filter-color-configuration
                            (red 255)
                            (green 255)
                            (blue 255)
                            (alpha 255)))
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
                            (shape 'Star))))

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
                            (red 0)
                            (green 0)
                            (blue 0)
                            (alpha 255)))
                          (set-border-color
                           (poe-item-filter-color-configuration
                            (red 0)
                            (green 0)
                            (blue 0)
                            (alpha 255)))
                          (set-background-color
                           (poe-item-filter-color-configuration
                            (red 249)
                            (green 150)
                            (blue 25)
                            (alpha 255)))
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
                            (shape 'Circle))))

                         (poe-item-filter-block-configuration
                          (base-types '("Divine Vessel"
                                        "Sacrifice at Dawn"
                                        "Sacrifice at Dusk"
                                        "Sacrifice at Midnight"
                                        "Sacrifice at Noon"
                                        "Offering To The Goddess"
                                        "Tribute To The Goddess"))
                          (set-text-color
                           (poe-item-filter-color-configuration
                            (red 178)
                            (green 120)
                            (blue 230)
                            (alpha 240)))
                          (set-border-color
                           (poe-item-filter-color-configuration
                            (red 175)
                            (green 120)
                            (blue 230)
                            (alpha 240)))
                          (minimap-icon
                           (poe-item-filter-minimap-icon-configuration
                            (enabled? #t)
                            (size 0)
                            (colour 'Red)
                            (shape 'Square)))
                          (play-effect
                           (poe-item-filter-play-effect-configuration
                            (colour 'Yellow))))

                         (poe-item-filter-block-configuration
                          (commentary "Highlight wombgifts.")
                          (classes '("Wombgifts"))
                          (set-text-color
                           (poe-item-filter-color-configuration
                            (red 0)
                            (green 0)
                            (blue 0)
                            (alpha 255)))
                          (set-border-color
                           (poe-item-filter-color-configuration
                            (red 0)
                            (green 0)
                            (blue 0)
                            (alpha 255)))
                          (set-background-color
                           (poe-item-filter-color-configuration
                            (red 120)
                            (green 200)
                            (blue 160)
                            (alpha 255)))
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
                            (shape 'Moon))))

                         (poe-item-filter-block-configuration
                          (commentary "Highlight Idols.")
                          (classes '("Idols"))
                          (set-text-color
                           (poe-item-filter-color-configuration
                            (red 255)
                            (green 178)
                            (blue 135)
                            (alpha 255)))
                          (set-border-color
                           (poe-item-filter-color-configuration
                            (red 255)
                            (green 178)
                            (blue 135)
                            (alpha 255)))
                          (set-background-color
                           (poe-item-filter-color-configuration
                            (red 20)
                            (green 20)
                            (blue 0)
                            (alpha 255)))
                          (play-effect
                           (poe-item-filter-play-effect-configuration
                            (colour 'Orange))))

                         (poe-item-filter-block-configuration
                          (classes '("Currency" "Incubators"))
                          (set-text-color
                           (poe-item-filter-color-configuration
                            (red 0)
                            (green 0)
                            (blue 0)
                            (alpha 255)))
                          (set-border-color
                           (poe-item-filter-color-configuration
                            (red 0)
                            (green 0)
                            (blue 0)
                            (alpha 255)))
                          (set-background-color
                           (poe-item-filter-color-configuration
                            (red 213)
                            (green 159)
                            (blue 0)
                            (alpha 255)))
                          (minimap-icon
                           (poe-item-filter-minimap-icon-configuration
                            (enabled? #t)
                            (size 2)
                            (colour 'White)
                            (shape 'Circle)))
                          (play-effect
                           (poe-item-filter-play-effect-configuration
                            (colour 'White))))

                         (poe-item-filter-block-configuration
                          (classes '("Amulet" "Belt" "Ring"))
                          (set-border-color
                           (poe-item-filter-color-configuration
                            (red 0)
                            (green 0)
                            (blue 0)
                            (alpha 255)))
                          (set-background-color
                           (poe-item-filter-color-configuration
                            (red 0)
                            (green 100)
                            (blue 150)
                            (alpha 255)))
                          (continue? #t))

                         (poe-item-filter-block-configuration
                          (base-types '("Amethyst Ring" "Gold Amulet" "Gold Ring" "Onyx Amulet"))
                          (set-border-color
                           (poe-item-filter-color-configuration
                            (red 0)
                            (green 0)
                            (blue 0)
                            (alpha 255)))
                          (set-background-color
                           (poe-item-filter-color-configuration
                            (red 38)
                            (green 0)
                            (blue 86)
                            (alpha 255)))
                          (continue? #t))

                         (poe-item-filter-block-configuration
                          (base-types '("Vanguard Belt"))
                          (play-effect
                           (poe-item-filter-play-effect-configuration
                            (colour 'Green)))
                          (continue? #t))

                         (poe-item-filter-block-configuration
                          (classes '("Incursion Items" "Labyrinth Items"))
                          (set-text-color
                           (poe-item-filter-color-configuration
                            (red 74)
                            (green 230)
                            (blue 58)
                            (alpha 255)))
                          (set-border-color
                           (poe-item-filter-color-configuration
                            (red 74)
                            (green 230)
                            (blue 58)
                            (alpha 255)))
                          (set-background-color
                           (poe-item-filter-color-configuration
                            (red 20)
                            (green 20)
                            (blue 0)
                            (alpha 255)))
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
                            (shape 'Pentagon))))

                         (poe-item-filter-block-configuration
                          (base-types '("Scarab"))
                          (set-text-color
                           (poe-item-filter-color-configuration
                            (red 255)
                            (green 0)
                            (blue 255)
                            (alpha 255)))
                          (set-border-color
                           (poe-item-filter-color-configuration
                            (red 255)
                            (green 0)
                            (blue 255)
                            (alpha 255)))
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
                            (shape 'Circle))))

                         (poe-item-filter-block-configuration
                          (classes '("Maps" "Map Fragments"))
                          (set-text-color
                           (poe-item-filter-color-configuration
                            (red 0)
                            (green 0)
                            (blue 0)
                            (alpha 255)))
                          (set-background-color
                           (poe-item-filter-color-configuration
                            (red 235)
                            (green 235)
                            (blue 235)
                            (alpha 255)))
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

                   (let ((jewel (poe-item-filter-block-configuration
                                 (set-text-color
                                  (poe-item-filter-color-configuration
                                   (red 0)
                                   (green 240)
                                   (blue 190)
                                   (alpha 255)))
                                 (set-border-color
                                  (poe-item-filter-color-configuration
                                   (red 0)
                                   (green 240)
                                   (blue 190)
                                   (alpha 255)))
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
                                   (shape 'Diamond))))))
                     (list (poe-item-filter-block-configuration
                            (inherit jewel)
                            (base-types '("Cobalt Jewel"
                                          "Crimson Jewel"
                                          "Viridian Jewel")))
                           (poe-item-filter-block-configuration
                            (inherit jewel)
                            (classes '("Abyss Jewels")))))

                   (let ((gem (poe-item-filter-block-configuration
                               (set-text-color
                                (poe-item-filter-color-configuration
                                 (red 30)
                                 (green 190)
                                 (blue 190)
                                 (alpha 255)))
                               (set-border-color
                                (poe-item-filter-color-configuration
                                 (red 30)
                                 (green 190)
                                 (blue 190)
                                 (alpha 255)))
                               (classes '("Skill Gems"
                                          "Support Gems"))
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
                                 (colour 'Grey))))))
                     (list
                      (poe-item-filter-block-configuration
                       (inherit gem)
                       (gem-level (poe-item-filter-conditional-value-configuration
                                   (value 20)
                                   (operator '>=))))

                      (poe-item-filter-block-configuration
                       (inherit gem)
                       (quality (poe-item-filter-conditional-value-configuration
                                 (value 1)
                                 (operator '>=))))

                      (poe-item-filter-block-configuration
                       (inherit gem)
                       (set-text-color
                        (poe-item-filter-color-configuration
                         (red 255)
                         (green 0)
                         (blue 0)
                         (alpha 255)))
                       (set-border-color
                        (poe-item-filter-color-configuration
                         (red 255)
                         (green 0)
                         (blue 0)
                         (alpha 255)))
                       (set-background-color
                        (poe-item-filter-color-configuration
                         (red 255)
                         (green 255)
                         (blue 255)
                         (alpha 255)))
                       (play-alert-sound
                        (poe-item-filter-play-alert-sound-configuration
                         (id 6)
                         (volume 300)))
                       (play-effect
                        (poe-item-filter-play-effect-configuration
                         (colour 'Red)))
                       (base-types '("Awakened"
                                     "Awakened Ancestral Call Support"
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
                                     "Awakened Void Manipulation Support")))))

                   (list
                    (poe-item-filter-block-configuration
                     (commentary "Highlight Rogue markers.")
                     (base-types '("Rogue's Marker"))
                     (set-text-color
                      (poe-item-filter-color-configuration
                       (red 255)
                       (green 178)
                       (blue 135)
                       (alpha 255)))
                     (set-border-color
                      (poe-item-filter-color-configuration
                       (red 255)
                       (green 178)
                       (blue 135)
                       (alpha 255)))
                     (set-background-color
                      (poe-item-filter-color-configuration
                       (red 20)
                       (green 20)
                       (blue 0)
                       (alpha 255)))
                     (play-effect
                      (poe-item-filter-play-effect-configuration
                       (colour 'Orange))))

                    (poe-item-filter-block-configuration
                     (commentary "Highlight Blueprints and Contracts and Sanctum.")
                     (classes '("Blueprints" "Contracts" "Sanctum Research"))
                     (set-text-color
                      (poe-item-filter-color-configuration
                       (red 255)
                       (green 85)
                       (blue 85)
                       (alpha 255)))
                     (set-border-color
                      (poe-item-filter-color-configuration
                       (red 255)
                       (green 85)
                       (blue 85)
                       (alpha 255)))
                     (set-background-color
                      (poe-item-filter-color-configuration
                       (red 40)
                       (green 0)
                       (blue 30)
                       (alpha 255)))
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
                       (shape 'UpsideDownHouse))))
                    (poe-item-filter-block-configuration
                     (commentary "Highlight not identified items.")
                     (identified? #f)
                     (rarity '(Magic Rare Unique))
                     (set-background-color (poe-item-filter-color-configuration
                                            (red 86)
                                            (green 0)
                                            (blue 0)
                                            (alpha 230)))
                     (continue? #t))
)

                   (delete #f
                           (apply append
                                  (map (lambda (type)
                                         (map (lambda (base-type)
                                                (if (and (string= base-type "Energy Shield")
                                                         (string= type "Body Armour"))
                                                    #f
                                                    (poe-item-filter-block-configuration
                                                     (commentary (format #f "Decrease font size for ~s ~s base items."
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
                                                     (set-background-color
                                                      (poe-item-filter-color-configuration
                                                       (red 0)
                                                       (green 0)
                                                       (blue 0)
                                                       (alpha 0)))
                                                     (show? #f)
                                                     (continue? #t))))
                                              exclude-sub-types))
                                       '("Body Armour"
                                         "Boots"
                                         "Gloves"
                                         "Helmet"))))

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
                                include-sub-types))

                   (list (poe-item-filter-block-configuration
                          (commentary "Increase font size for high level sceptres base items.")
                          (base-types '("Opal Sceptre" "Void Sceptre"))
                          (set-font-size 45)
                          (continue? #t))

                         (poe-item-filter-block-configuration
                          (commentary "Increase font size for high level wands base items.")
                          (base-types '("Opal Wand" "Profane Wand"))
                          (set-font-size 45)
                          (continue? #t))

                         (poe-item-filter-block-configuration
                          (commentary "Decrease font size for items with classes.")
                          (classes %weapon-classes)
                          (set-font-size 20)
                          (show? #f)
                          (set-background-color
                           (poe-item-filter-color-configuration
                            (red 0)
                            (green 0)
                            (blue 0)
                            (alpha 0)))
                          (continue? #t))

                         (poe-item-filter-block-configuration
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
                          (continue? #t))

                         (let ((color (poe-item-filter-color-configuration
                                       (red 140)
                                       (green 60)
                                       (blue 25)
                                       (alpha 255))))
                             (poe-item-filter-block-configuration
                          (classes '("Utility Flasks"))
                          (set-border-color color)
                          (set-background-color color)
                          (continue? #t)))

                         (poe-item-filter-block-configuration
                          (classes '("Life Flasks" "Mana Flasks" "Hybrid Flasks"))
                          (rarity '(Normal))
                          (show? #f)
                          (item-level (poe-item-filter-conditional-value-configuration
                                       (value 83)
                                       (operator '<=)))
                          (continue? #t))

                         (poe-item-filter-block-configuration
                          (classes '("Life Flasks" "Mana Flasks" "Hybrid Flasks" "Utility Flasks"))
                          (quality (poe-item-filter-conditional-value-configuration
                                    (value 1)
                                    (operator '>=)))
                          (play-effect
                           (poe-item-filter-play-effect-configuration
                            (colour 'Green))))

                         (poe-item-filter-block-configuration
                          (commentary "Hide identified items.")
                          (identified? #t)
                          (show? #f))))))
         poe-item-filter-configuration-fields)))

(run-with-store (open-connection)
  (text-file* "wigust.filter" (generate-filter)))
