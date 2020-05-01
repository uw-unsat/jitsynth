#lang rosette

(define all-out-file null)
(define indiv-out-file null)

; csv lines have 5 or 6 things in them
; 5 is per atom (unless 2nd arg is "all")
;   instr,output length,cpu-time,real-time,garbage-collection-time
; 6 is overall (remove "all")
;   instr,output length,cpu-time,real-time,garbage-collection-time
; Output file is "Config: X" --> S2T-X.csv

(define file-prefix (~a (vector-ref (current-command-line-arguments) 0)))
(define file-lines (file->lines (~a file-prefix ".out")))

(define (write-all-file line)
  (displayln line all-out-file))

(define (write-indiv-file line)
  (displayln line indiv-out-file))

(define (update-out-file config-line)
  (let* ([config (second (string-split config-line #px": "))]
         [all-name (~a file-prefix "-all-" config ".csv")]
         [indiv-name (~a file-prefix "-indiv-" config ".csv")])
    (when (not (null? all-out-file))
      (close-output-port all-out-file))
    (when (not (null? indiv-out-file))
      (close-output-port indiv-out-file))
    (set! all-out-file (open-output-file all-name #:exists 'replace))
    (set! indiv-out-file (open-output-file indiv-name #:exists 'replace))
    (write-all-file "bench,cpu,real")
    (write-indiv-file "bench,output-length,cpu,real,status")))

(define failed? (make-hash))
; (define src-machine-time #f)
; (define tgt-machine-time #f)

(for ([line file-lines])
  (let* ([space-args (string-split line)]
         [comma-args (string-split line ",")])
    (displayln space-args)
    (cond
      #|
      [(and (string-contains? line "machine time: ")
            (or (not src-machine-time)
                (not tgt-machine-time)))
       (if (not src-machine-time)
         (set! src-machine-time (string->number (string-trim (fifth space-args) #px"(\\s|s)+")))
         (set! tgt-machine-time (string->number (string-trim (fifth space-args) #px"(\\s|s)+"))))
       (when (and src-machine-time tgt-machine-time)
         (write-all-file (~a "machines,na," (* 1000 (+ src-machine-time tgt-machine-time)))))]
      |#
      [(string-prefix? line "Starting synthesis for instruction")
       (set! failed? (make-hash))]
      [(string-prefix? line "Skipping atom")
       (hash-set!
         failed?
         (third space-args)
         "success!")
       (displayln failed?)]
      [(string-prefix? line "Done with synth for atom")
       (hash-set!
         failed?
         (string-trim (sixth space-args) #px"(\\s|:)+")
         (seventh space-args))
       (displayln failed?)]
      [(or (= 5 (length comma-args))
           (= 6 (length comma-args)))
       (cond
         [(equal? "all" (second comma-args))
          (write-all-file (~a (first comma-args) ","
                              (third comma-args) ","
                              (fourth comma-args)))]
         [(equal? "all" (third comma-args))
          (write-all-file (~a (first comma-args) ","
                              (fourth comma-args) ","
                              (fifth comma-args)))]
         [else
           (write-indiv-file
             (~a (first comma-args) ","
                 (second comma-args) ","
                 (third comma-args) ","
                 (fourth comma-args) ","
                 (hash-ref failed? (second comma-args) "failed")))])]
      [(and (= 2 (length space-args))
            (equal? "Config:" (first space-args)))
       (update-out-file line)])))
