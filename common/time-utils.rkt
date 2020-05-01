#lang rosette

(provide time-and-output)

(define-syntax time-and-output
  (syntax-rules ()
    [(time-and-output x)
     (time-and-output x "" (current-output-port))]
    [(time-and-output x preamb)
     (time-and-output x preamb (current-output-port))]
    [(time-and-output x preamb out-port)
     (let-values
       ([(outlist cpu-time real-time garbage-time)
         (time-apply (thunk* x) null)])
       (displayln (~a preamb cpu-time "," real-time "," garbage-time) out-port)
       outlist)]))
