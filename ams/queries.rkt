#lang rosette

(require "../gensketch/gensketch-rws.rkt"
         "../common/data-structures.rkt"
         "../common/data-utils.rkt"
         "../common/symbolic-ops.rkt"
         "../synthesis/query-utils.rkt"
         "machine.rkt")
(require (only-in "../common/debug.rkt" [debugln common:debugln]))
(require rosette/lib/angelic)

(provide synth-pc-adjust synth-pc-store synth-mem-store
         synth-mem-store-precise synth-pc-load synth-mem-load
         synth-mem-load-precise synth-param-load synth-return
         synth-zero-extend synth-sign-extend)

(define DEBUG #f)

(define (debugln x)
  (if DEBUG (common:debugln x) void))

(define (synth-helper target rs ws desired-state-func
                      [forall-vars null]
                      #:assumptions [assumptions null]
                      #:max-size [max-size 3]
                      #:with-assumpts [with-assumpts #f]
                      #:alt-value [value #f])
  (debugln "started the let")
  (let* ([start-state (machine-default-state target)]
         [iws (get-iwss target)]
         [sketch-func
           (if value
             (make-gensketch-rws-with-value target rs ws value)
             (make-gensketch-rws target rs ws))]
         [assume-func
           (lambda (sketch)
             (let* ([nop-state (make-nop-state target (length sketch))]
                    [desired-state (desired-state-func nop-state start-state)])
               (for ([asmt assumptions])
                 (assert asmt))
               (assert (bveq (bvand (bv 1 64) (state-pc start-state)) (bv 0 64)))))]
         [guarantee-func 
           (lambda (sketch)
             (let* ([nop-state (make-nop-state target (length sketch))]
                    [desired-state (desired-state-func nop-state start-state)]
                    [end-state (iws sketch start-state (bv64 (length sketch)))])
               (assert (state-equal? target end-state desired-state))))])
    (debugln "finished the let")
    (define-values (result result-asmts)
      (synth-from-gensketch
        sketch-func
        (flatten-list
          forall-vars
          (get-equality-indicies target)
          symbolic-bvops
          start-state)
        #:assume-func assume-func
        guarantee-func
        max-size))
    (debugln "finished the results")
    (if with-assumpts
      (values result result-asmts)
      result)))

(define (synth-pc-adjust target adj-val desired-pc)
  (debug-on-unsat
    "synth-pc-adjust" DEBUG
    (synth-helper
      target (list 'regs 'pc) (list 'pc)
      (lambda (nop-state start-state)
        (make-state (state-regs nop-state) (state-mem nop-state) desired-pc)))))

(define (synth-pc-store target pc-reg)
  (debug-on-unsat
    "synth-pc-store" DEBUG
    (synth-helper target (list 'regs) (list 'pc 'regs)
                  (lambda (nop-state start-state)
                    (make-state (state-regs nop-state) (state-mem nop-state)
                                (bvand (bv -2 64) (read-register (state-regs start-state) pc-reg)))))))

(define (synth-mem-store target mem-reg mem-res)
  (debug-on-unsat
    "synth-mem-store" DEBUG
    (synth-helper target (list 'regs) (list 'mem)
                  (lambda (nop-state start-state)
                    (make-state (state-regs nop-state) mem-res (state-pc nop-state))))))

(define (synth-mem-store-precise target mem-reg mem-ind mem-len)
  (debug-on-unsat
    "synth-mem-store-precise" DEBUG
    (synth-helper target (list 'regs) (list 'mem)
                  (lambda (nop-state start-state)
                    (make-state (state-regs nop-state)
                                (write-memory
                                  (state-mem start-state)
                                  mem-ind mem-len
                                  (read-register (state-regs start-state) mem-reg))
                                (state-pc nop-state))))))

(define (synth-return target)
  (debug-on-unsat
    "synth-retutrn" DEBUG
    (synth-helper target empty (list 'return?)
                  #:max-size 1
                  (lambda (nop-state start-state)
                    (make-state (state-regs nop-state)
                                (state-mem nop-state)
                                (state-pc nop-state)
                                #t)))))

; NOTE: This one is kinda special since the PC can change,
;       so it NEEDS to be the first thing that's loaded
(define (synth-pc-load target pc-reg)
  (debug-on-unsat
    "synth-pc-load" DEBUG
    (synth-helper target (list 'pc) (list 'regs)
                  (lambda (nop-state start-state)
                    (make-state (write-register
                                  (state-regs start-state)
                                  pc-reg (state-pc start-state))
                                (state-mem nop-state)
                                (state-pc nop-state))))))

(define (synth-mem-load target mem-reg reg-res)
  (debug-on-unsat
    "synth-mem-load" DEBUG
    (synth-helper target (list 'mem) (list 'regs)
                  (lambda (nop-state start-state)
                    (make-state reg-res (state-mem nop-state) (state-pc nop-state))))))

(define (synth-mem-load-precise target mem-reg mem-ind mem-len)
  (debug-on-unsat
    "synth-mem-load-precise" DEBUG
    (synth-helper target (list 'mem) (list 'regs)
                  (lambda (nop-state start-state)
                    (make-state (state-regs nop-state)
                                (write-memory
                                  (state-mem start-state)
                                  mem-ind mem-len
                                  (read-register (state-regs start-state) mem-reg))
                                (state-pc nop-state))))))

(define (get-bv-func val bitlen thunk-bvop max-depth)
  (if (= 0 max-depth)
    val
    (choose* val ((thunk-bvop) (get-bv-func val bitlen thunk-bvop (sub1 max-depth))
                         (choose* val (symbv bitlen))))))

(define (get-param-new-value val bitlen)
  (let* ([bvop-list (list bvand bvadd bvshl bvor bvxor bvlshr bvashr)]
         [bvop (thunk (apply choose* bvop-list))]
         [nval (get-bv-func val bitlen bvop 3)])
    nval))

; NOTE: So that this is only called once,
;       the same symbolic should be used for all source instructions
(define (synth-param-load
          src/tgt-pair param-reg param-value
          largest-prog-len bitlen try-scale?)
  (define target (src/tgt-target src/tgt-pair))
  (define pc-fact (src/tgt-pc-fact src/tgt-pair))
  (define param-value-extended (sign-extend param-value (bitvector bitlen)))
  (define scales
    (if try-scale?
      (list (bv 1 bitlen)
            (bv (* pc-fact largest-prog-len) bitlen))
      (list (bv 1 bitlen))))
  (define adjusts
    (if try-scale?
      (list (bv 0 bitlen)
            (bv (* pc-fact largest-prog-len) bitlen))
      (list (bv 0 bitlen))))
  (define param-value-adjusts
    (map (lambda (ind)
           (bvadd (bvmul (list-ref scales ind) param-value-extended)
                  (list-ref adjusts ind)))
         (range (length scales))))
  (define results-with-asmts
    (for/list ([param-value-adj param-value-adjusts])
      (begin
        (define-values (new-param-value new-param-asserts)
          (with-asserts (get-param-new-value param-value-adj bitlen)))
        (define-values (my-res my-asmts)
          (if (not param-reg)
            (values (unsat) null)
            (synth-helper target null (list 'regs)
                          #:max-size 2
                          #:alt-value new-param-value
                          #:with-assumpts #t
                          #:assumptions new-param-asserts
                          (lambda (nop-state start-state)
                            (make-state (write-register
                                          (state-regs start-state)
                                          param-reg
                                          (sign-extend param-value-adj (bitvector 64)))
                                        (state-mem nop-state)
                                        (state-pc nop-state)))
                          (list param-value-adj))))
        (cons my-res my-asmts))))
  (define results (map car results-with-asmts))
  (define result-asmts (map cdr results-with-asmts))
  (debug-on-unsat "synth-param-load" DEBUG results)
  (values results result-asmts))

; NOTE: tgt-regs are a list of reigsters that the source instruction uses
(define (synth-zero-extend source target tgt-regs)
  (define (make-regs st reg-list)
    (if (empty? reg-list) st
      (make-regs
        (write-register
          st (first reg-list)
          (zero-extend
            (extract (sub1 (get-regs-bitlen source)) 0
                     (read-register st (first reg-list)))
            (bitvector (get-regs-bitlen target))))
        (rest reg-list))))
  (define-values (result result-asmts)
    (synth-helper target '(regs) '(regs)
                  #:max-size 2
                  #:with-assumpts #t
                  (lambda (nop-state start-state)
                    (make-state
                      (make-regs (state-regs start-state) tgt-regs)
                      (state-mem nop-state)
                      (state-pc nop-state)))))
  (debug-on-unsat "synth-zero-extend" DEBUG result)
  (values result result-asmts))

; NOTE: tgt-regs are a list of reigsters that the source instruction uses
(define (synth-sign-extend source target tgt-regs)
  (debugln (~a "tgt regs: " tgt-regs))
  (define (make-regs st reg-list)
    (if (empty? reg-list) st
      (make-regs
        (write-register
          st (first reg-list)
          (sign-extend
            (extract (sub1 (get-regs-bitlen source)) 0
                     (read-register st (first reg-list)))
            (bitvector (get-regs-bitlen target))))
        (rest reg-list))))
  (define-values (result result-asmts)
    (synth-helper target '(regs) '(regs)
                  #:max-size 2
                  #:with-assumpts #t
                  (lambda (nop-state start-state)
                    (make-state
                      (make-regs (state-regs start-state) tgt-regs)
                      (state-mem nop-state)
                      (state-pc nop-state)))))
  (debug-on-unsat "synth-sign-extend" DEBUG result)
  (values result result-asmts))
