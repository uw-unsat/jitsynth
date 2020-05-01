#lang rosette

(provide debugln)

(define DEBUG #f)

(define (debugln x [label ""])
  (if DEBUG (displayln (~a label x)) (void))
  (flush-output)
  x)
