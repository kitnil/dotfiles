(define-module (rss code)
  #:use-module (feed rss))

(define-public abo-abo
  (feed
   (name "abo-abo")
   (description "Oleh Krehel blog.")
   (uri "https://oremacs.com/atom.xml")))

(define-public nullprogram
  (feed
   (name "nullprogram")
   (description "Chris Wellons blog.")
   (uri "http://nullprogram.com/feed/")))

(define-public steckerhalter
  (feed
   (name "steckerhalter")
   (description "steckerhalter blog.")
   (uri "http://steckerhalter.tk/index.xml")))
