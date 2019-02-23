(define-module (rss hardware)
  #:use-module (feed rss))

(define-public h-node
  (feed
   (name "h-node")
   (description "Libre hardware list.")
   (uri "https://h-node.org/rss/modifications/en")))
