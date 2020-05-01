#lang rosette

(require "../gensketch/gensketch-rws.rkt"
         "../ams/machine.rkt"
         "../common/debug.rkt"
         "../common/options.rkt"
         "../common/data-utils.rkt"
         "../common/data-structures.rkt")
(require (only-in "../common/debug.rkt" [debugln common:debugln]))
(require rosette/lib/angelic
         racket/engine)

(provide error-on-unsat debug-on-unsat
         synth-from-gensketch synth-from-gensketch-thunk)

(define DEBUG #f)

(define (debugln x)
  (if DEBUG (common:debugln x) (void)))

(define (error-on-unsat method-name mdl)
  (if (unsat? mdl)
    (error (~a "Unsat returned for call to " method-name))
    mdl))

(define (debug-on-unsat method-name debug? value)
  (if (and debug? (unsat? value))
    (displayln (~a "Unsat returned for call to " method-name))
    (void))
  value)

(define (synth-from-gensketch-helper
          gensketch-func forall-vars
          assume-func guarantee-func
          size max-size with-mdl?)
  (clear-asserts!)
  (if (and (not (equal? -1 max-size)) (> size max-size))
    (if with-mdl? (values (unsat) null null) (values (unsat) null))
    (begin
      (debugln (~a "Trying synthesis with sketch size " size))
      (let*-values
        ([(sketch sketch-asmts) (with-asserts (gensketch-func size))])
        ; TODO This check seems not to work
        (if (unsat? sketch)
          (synth-from-gensketch-helper
            gensketch-func forall-vars
            assume-func guarantee-func
            (add1 size) max-size with-mdl?)
          (let*
            ([assume-func-asmts (with-asserts-only (assume-func sketch))]
             [mdl-func
               (thunk* ; TODO take an argument here for killing something?
                 (with-handlers
                   ([exn:fail?
                      (lambda (err)
                        (displayln (~a "Synthesizer error: " err))
                        (unsat))])
                   (synthesize
                     #:forall forall-vars
                     #:assume
                     (begin
                       (for ([asmt sketch-asmts])
                         (assert asmt))
                       (for ([asmt assume-func-asmts])
                         (assert asmt)))
                     #:guarantee (guarantee-func sketch))))]
             [eng (engine mdl-func)]
             [run-res (if (>= QUERY_TIMEOUT 0)
                        (engine-run QUERY_TIMEOUT eng)
                        #t)]
             [mdl
               (cond
                 [(and run-res (>= QUERY_TIMEOUT 0))
                  (engine-result eng)]
                 [run-res (mdl-func)]
                 [else
                   (debugln "Timed out")
                   (engine-kill eng)
                   (unsat)])])
            (if (unsat? mdl)
              (synth-from-gensketch-helper
                gensketch-func forall-vars
                assume-func guarantee-func
                (add1 size) max-size with-mdl?)
              (if with-mdl?
                ; TODO want mdl to be at the end instead?
                (values (evaluate sketch mdl) mdl (flatten-list sketch-asmts assume-func-asmts))
                (values (evaluate sketch mdl) (flatten-list sketch-asmts assume-func-asmts))))))))))

; Returns (unsat) when synthesis is not possible
(define (synth-from-gensketch
          gensketch-func forall-vars
          guarantee-func [max-size COMPUTE_BOUND]
          #:assume-func [assume-func void]
          #:with-model? [with-mdl? #f])
  (debugln (~a "Max size: " max-size))
  (synth-from-gensketch-helper
    gensketch-func
    forall-vars
    assume-func
    guarantee-func
    0
    ; NOTE: This limits the size of computation
    (min max-size COMPUTE_BOUND)
    with-mdl?))

(define (synth-from-gensketch-thunk-helper
          gensketch-func forall-vars
          guarantee-thunk-func size max-size)
  (if (> size max-size) (unsat)
    (let* ([sketch (gensketch-func size)]
           [guarantee-thunk (guarantee-thunk-func sketch)]
           [mdl (synthesize
                  #:forall forall-vars
                  #:guarantee (guarantee-thunk))])
      (if (unsat? mdl)
        (synth-from-gensketch-helper
          gensketch-func forall-vars
          guarantee-thunk-func (add1 size) max-size)
        (evaluate sketch mdl)))))

(define (synth-from-gensketch-thunk
          gensketch-func
          forall-vars
          guarantee-thunk-func
          [max-size 3])
  (synth-from-gensketch-thunk-helper gensketch-func forall-vars guarantee-thunk-func 0 max-size))
