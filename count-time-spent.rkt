#lang rosette

(define file-name (~a (vector-ref (current-command-line-arguments) 0)))
(define file-lines (file->lines file-name))

(define total-time
  (for/sum ([line file-lines])
    (let* ([comma-args (string-split line ",")])
      (cond
        [(and (= (length comma-args) 5)
              (equal? "all" (second comma-args)))
         (string->number (fourth comma-args))]
        [(and (= (length comma-args) 6)
              (equal? "all" (third comma-args)))
         (string->number (fifth comma-args))]
        [else 0]))))

(displayln (~a "Took " (quotient total-time 1000) " seconds,"))
(displayln (~a "  or " (~r (/ total-time   60000.0) #:precision 2) " minutes,"))
(displayln (~a "  or " (~r (/ total-time 3600000.0) #:precision 2)" hours."))
