(define-module (fiore modules utils)
  #:use-module (srfi srfi-1)
  #:export (cartesian-product))

(define (cartesian-product . lists)
  (fold-right (lambda (xs ys)
                (append-map (lambda (x)
                              (map (lambda (y)
                                     (cons x y))
                                   ys))
                            xs))
              '(())
              lists))
