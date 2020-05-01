#lang rosette

(require "lsc-dsl.rkt"
         "interpreter.rkt"
         "../common/instructions.rkt"
         "../ams/machine.rkt"
         "../common/data-structures.rkt")

(require (only-in "../common/debug.rkt" [debugln common:debugln]))
(require (only-in "../bpf/interpreter.rkt" [interpret-with-state interpret-bpf-with-state]))

(require rosette/lib/synthax)
(require rosette/lib/angelic)
(require rosette/solver/smt/boolector)

(define interpret-lsc-with-state interpret-with-state)

(current-solver (boolector #:path "../boolector/"))

(define DEBUG #t)

(define (debugln x)
  (if DEBUG
    (common:debugln x)
    (void)))

(define (bv8 i) (bv i 8))
(define (bv32 i) (bv i 32))
(define zero (bv32 0))

(define (symbv len)
  (define-symbolic* sbv (bitvector len))
  sbv)

; TODO there are more
(define lsc-comparators
  (list bveq bvugt bvule bvsge bvslt))

; List of triples (func, srcbit, cls)
(define bpf-jumps
  (map (lambda (code)
         (let ([op (bv8 code)])
           (list 
             (extract 7 4 op)
             (extract 3 3 op)
             (extract 2 0 op))))
       (list #x05 #x15 #x1d #x25 #x2d #x35 #x3d #xa5 #xad #xb5 #xbd
             #x45 #x4d #x55 #x5d #x65 #x6d #x75 #x7d #xc5 #xcd #xd5 #xdd)))

(define bpf-ops
  (map (lambda (code)
         (let ([op (bv8 code)])
           (list 
             (extract 7 4 op)
             (extract 3 3 op)
             (extract 2 0 op))))
       (list #x07 #x0f #x17 #x1f #x27 #x2f #x37 #x3f #x47 #x4f #x57 #x5f
             #x67 #x6f #x77 #x7f #x87 #x97 #x9f #xa7 #xaf #xb7 #xbf #xc7
             #xcf #x18 #x61 #x69 #x71 #x79 #x62 #x6a #x72 #x7a #x63 #x6b
             #x73 #x7b #x05 #x15 #x1d #x25 #x2d #x35 #x3d #xa5 #xad #xb5
             #xbd #x45 #x4d #x55 #x5d #x65 #x6d #x75 #x7d #xc5 #xcd #xd5 #xdd
             #x04 #x0c #x14 #x1c #x24 #x2c #x34 #x3c #x44 #x4c #x54 #x5c #x64
             #x6c #x74 #x7c #x84 #x94 #x9c #xa4 #xac #xb4 #xbc #xc4 #xcc)))

(define (bpf-jump)
  (let ([jump (apply choose* bpf-jumps)])
    (bpf-instr
      ; TODO for speed, maybe replace (symbv 4) with reg0
      (symbv 32) (symbv 16) (symbv 4) (symbv 4)
      ; TODO Choose from all regs?
      (first jump) (second jump) (third jump))))
    
(define (bpf-op)
  (let ([op (apply choose* bpf-ops)])
    (bpf-instr
      ; TODO for speed, maybe replace (symbv 4) with reg0
      (symbv 32) (symbv 16) (symbv 4) (symbv 4)
      ; TODO Choose from all regs?
      (first op) (second op) (third op))))
    
(define bpf-ret
  (bpf-instr (bv32 0) (bv 0 16) (bv 0 4) (bv 0 4) (bv 9 4) (bv 0 1) (bv 5 3)))

; NOTE: Supposing the value is 32-bit
(define (bpf-const-load reg value)
  (list
    (bpf-instr value (bv 0 16) (bv 0 4) reg (bv #xb 4) (bv 0 1) (bv 7 3))))

(define (bpf-mem-load reg mem-index)
  (list
    ; TODO may want to replace (symbv 4) with concrete reg for speed
    (bpf-instr (bv32 0) mem-index (symbv 4) reg (bv 7 4) (bv 1 1) (bv 1 3))))

(define scratch-regs
  (list (bv 4 4)
        (bv 5 4)
        (bv 6 4)
        (bv 7 4)
        (bv 8 4)))

; TODO
(define (arg-sketch arg)
  (flatten (list
    ; load arg value into register (memory load)
    ; (bpf-instr (bv32 0) (bv 8 16) (bv 2 4) (bv 4 4) (bv 7 4) (bv 1 1) (bv 1 3))
    (bpf-mem-load (first scratch-regs) (bvshl (bvadd (bv 1 16) (db-api-arg-arg arg)) (bv 3 16)))

    ; load arg datum into register (constant load)
    ; (bpf-instr (db-api-arg-datum arg) (bv 0 16) (bv 0 4) (bv 5 4) (bv #xb 4) (bv 0 1) (bv 7 3))
    (bpf-const-load (second scratch-regs) (db-api-arg-datum arg)) 

    ; conditional jump of +1 based on the comparison
    ; (bpf-instr (bv32 0) (bv 2 16) (bv 4 4) (bv 5 4) (bv 1 4) (bv 1 1) (bv 5 3))
    ; (bpf-jump)
    (bpf-op)

    ; if not jumped (not passed), set r1 to 1
    ; (bpf-instr (bv32 1) (bv 0 16) (bv 0 4) (bv 1 4) (bv #xb 4) (bv 0 1) (bv 7 3)))))
    (bpf-const-load (symbv 4) (symbv 32))))); (bv 1 4) (bv 1 32)))))

(define (rule-sketch rule)
  (flatten (list
    ; load syscall
    ; (bpf-instr (bv32 0) (bv 0 16) (bv 2 4) (bv 4 4) (bv 7 4) (bv 1 1) (bv 1 3))
    (bpf-mem-load (first scratch-regs) (bv 0 16))

    ; load rule-call
    (bpf-const-load (second scratch-regs) (db-api-rule-syscall rule))

    ; load action (into reg 0!, kinda cheating)
    (bpf-const-load (third scratch-regs) (db-api-rule-action rule))

    ; cond jump based on has-failed-rule?
    ; (bpf-instr (bv32 1) (bv 5 16) (bv 0 4) (bv 1 4) (bv 1 4) (bv 0 1) (bv 5 3))
    ; (bpf-jump)
    (bpf-op)

    ; cond jump based on syscall==rule-call
    ; (bpf-instr (bv32 0) (bv 5 16) (bv 4 4) (bv 5 4) (bv 5 4) (bv 0 1) (bv 5 3))
    ; (bpf-jump)
    (bpf-op)

    ; Move action into return register
    (bpf-instr (bv32 0) (bv 0 16) (symbv 4) (symbv 4) (bv #xb 4) (bv 1 1) (bv 7 3))

    ; return
    bpf-ret)))

(define (default-sketch action)
  (flatten (list
    ; load action
    (bpf-const-load (symbv 4) action)

    ; return
    bpf-ret)))

; Equality measure
(define (regs-equal? lsc-regs bpf-regs)
  ; NOTE: Assuming same register structure
  (foldl
   (lambda (index acc)
     (and (bveq (read-register lsc-regs index)
                (read-register bpf-regs index))
          acc))
   #t
   (build-list 2 bv8)))

(define (state-equal-lsc? lsc-state bpf-state)
  (regs-equal? (state-rvals lsc-state) (state-rvals bpf-state)))
; TODO others

(define (arg-num)
  (apply choose* (map bv32 (list 0 1 2 3 4 5))))

(define datum (symbv 32))
; (define datum (bv32 1))

; TODO disparity between 32-bit and 64-bit?
;      should allow for constant to be 64-bit
; (define my-arg (db-api-arg (arg-num) (apply choose* lsc-comparators) (bv32 0) (symbv 32) #f))
;                          arg      op   mask     datum      valid
(define my-arg (db-api-arg (bv 0 16) bveq (bv32 0) datum #f))
(define my-default (symbv 32))
; (struct db-api-rule (action syscall strict args) #:transparent)
(define my-rule (db-api-rule (symbv 64) (symbv 64) #f #f))

; Arg instr
; (define my-instr (lsc-instruction 'arg my-arg))
; (define my-sketch (arg-sketch my-arg))

; Default instr
; (define my-instr (lsc-instruction 'default my-default))
; (define my-sketch (default-sketch my-default))

; Rule instr
(define my-instr (lsc-instruction 'rule my-rule))
(define my-sketch (rule-sketch my-rule))

(define sketch-len (length my-sketch))
(define pcfact (bv sketch-len 64))


(define regs (make-registers 11))
(define mem (make-memory 256))
(define-symbolic pc (bitvector 64))
; (define regs (write-register (write-register (make-zerod-registers 11) (bv 1 8) (bv 1 64)) (bv 0 8) (bv 1 64)))
; (define mem (make-zerod-memory 256))
; (define pc (bv 0 64))
(define bpf-pc (bvmul pc pcfact))

; (debugln my-sketch)
(define (ri i state) (read-register (state-rvals state) (bv i 8)))
(define (mi i state) (read-memory (state-memory state) (bv i 8) (bv 8 64)))

; Assumptions
(assert (or (bveq (read-register regs (bv 1 8)) (bv 0 64))
            (bveq (read-register regs (bv 1 8)) (bv 0 64))))
(assert (bveq (read-register regs (bv 2 8)) (bv 0 64)))

; NOTE: Ideally, will not need this
(define lsc-state (interpret-lsc-with-state (list my-instr) regs mem pc (bv 1 64)))
(debugln "Done interpreting lsc")
; (define bpf-state (interpret-bpf-with-state my-sketch regs mem bpf-pc pcfact))
#|
(for/all ([val (ri 1 bpf-state) #:exhaustive])
  (debugln (~a "Val: " val)))
|#
; (error-print-width 100000000)
; (debugln (state-equal-lsc? lsc-state bpf-state))
; (for ([as (asserts)])
;   (debugln (~a "Assert: " as)))
(define asmt
  (synthesize
    #:forall (list my-instr regs mem pc)
    #:guarantee
    (let ([bpf-state (interpret-bpf-with-state my-sketch regs mem bpf-pc pcfact)])
      (assert (state-equal-lsc? lsc-state bpf-state)))))
(if (unsat? asmt)
  (debugln "Unable to synthesize")
  (debugln (~a "Prog: " (evaluate my-sketch asmt))))

#|
(set! asmt (verify (assert (state-equal-lsc? lsc-state bpf-state))))
(debugln asmt)
; (debugln (~a "Datum: " (evaluate datum asmt)))
; (debugln (~a "Argval: " (mi 8 lsc-state)))
; (debugln (~a "BPF R4: " (ri 4 bpf-state)))
; (debugln (~a "Argval: " (evaluate (mi 8 lsc-state))))
(debugln (~a "BPF R4: " (evaluate (ri 4 bpf-state) asmt)))
(debugln (~a "BPF R5: " (evaluate (ri 5 bpf-state) asmt)))
(debugln (~a "LSC R0: " (evaluate (ri 0 lsc-state) asmt)))
(debugln (~a "LSC R1: " (evaluate (ri 1 lsc-state) asmt)))
(debugln (~a "BPF R0: " (evaluate (ri 0 bpf-state) asmt)))
(debugln (~a "BPF R1: " (evaluate (ri 1 bpf-state) asmt)))
(debugln (~a "State equal: " (evaluate (state-equal-lsc? lsc-state bpf-state) asmt)))
|#
