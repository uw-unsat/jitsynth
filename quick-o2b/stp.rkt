#lang rosette

(require "../synthesis/compiler-query.rkt"
         "../ams/machine.rkt"
         "../common/data-structures.rkt"
         "../common/case-hex.rkt"
         "../common/data-utils.rkt"
         "../bpf/machine-instance.rkt"
         "../quick-old-bpf/machine-instance.rkt")

(require (only-in "../common/debug.rkt" [debugln common:debugln]))

(provide make-stp)

(define DEBUG #f)

(define (debugln x)
  (if DEBUG (void (common:debugln x)) void))

; NOTE: The name of this function is important
(define (make-stp
          [obpf-instrs #f]
          [ebpf-instrs #f])
  (debugln "Loading BPF machine")
  (define start-time (current-seconds))
  (define bpf-machine
    (if ebpf-instrs
      (make-bpf-machine ebpf-instrs)
      (make-bpf-machine)))

  (debugln (~a "Loading BPF machine time: " (- (current-seconds) start-time) "s"))

  (debugln "Loading Old BPF machine")
  (set! start-time (current-seconds))
  (define old-bpf-machine (make-quick-old-bpf-machine))

  (debugln (~a "Loading Old BPF machine time: " (- (current-seconds) start-time) "s"))

  (define (old-bpf-reg-index->bpf index)
    (case-hex index 8
              [(0) (bv 0 8)]
              [(1) (bv 1 8)]
              [(2) (bv 2 8)]))

  (define (bpf-regs->old-bpf regs)
    (make-store-from-existing
      regs old-bpf-reg-index->bpf
      3 8 32))

  (define (bpf-mem->old-bpf mem)
    (make-store-from-existing mem identity 16))

  (define scratch-regs
    (map (bvl 4) (list 5 6 7 8 9)))

  (define usable-regs
    (build-list 10 (bvl 4)))

  (src/tgt
    old-bpf-machine
    bpf-machine
    scratch-regs
    usable-regs
    old-bpf-reg-index->bpf
    bpf-regs->old-bpf
    bpf-mem->old-bpf
    identity
    1
    64))
