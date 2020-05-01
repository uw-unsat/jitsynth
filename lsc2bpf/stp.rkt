#lang rosette

(require "../synthesis/compiler-query.rkt"
         "../ams/machine.rkt"
         "../common/data-structures.rkt"
         "../common/case-hex.rkt"
         "../common/data-utils.rkt"
         "../bpf/machine-instance.rkt"
         "../lsc/machine-instance.rkt")

(require (only-in "../common/debug.rkt" [debugln common:debugln]))

(provide make-stp)

(define DEBUG #f)

(define (debugln x)
  (if DEBUG (void (common:debugln x)) void))

(define (make-stp
          [lsc-instrs #f]
          [ebpf-instrs #f])
  (debugln "Loading BPF machine")
  (define start-time (current-seconds))
  (define bpf-machine
    (if ebpf-instrs
      (make-bpf-machine ebpf-instrs)
      (make-bpf-machine)))

  (debugln (~a "Loading BPF machine time: " (- (current-seconds) start-time) "s"))

  (debugln "Loading LSC machine")
  (set! start-time (current-seconds))
  (define lsc-machine
    (if lsc-instrs
      (make-lsc-machine lsc-instrs)
      (make-lsc-machine)))

  (debugln (~a "Loading LSC machine time: " (- (current-seconds) start-time) "s"))

  (define s2t-reg-index
    (lambda (index)
      (case-hex index 8
                [(0) (bv 0 8)]
                [(1) (bv 1 8)]
                [(2) (bv 2 8)])))

  (define (bpf-regs->lsc regs)
    (make-store-from-existing
      regs s2t-reg-index 3))

  (define scratch-regs
    (map (bvl 4) (list 5 6 7 8 9)))

  (define usable-regs
    (build-list 10 (bvl 4)))

  (src/tgt
    lsc-machine
    bpf-machine
    scratch-regs
    usable-regs
    s2t-reg-index
    bpf-regs->lsc
    identity
    identity
    1
    64))
