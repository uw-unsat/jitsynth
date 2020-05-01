#lang rosette

(require "../synthesis/compiler-query.rkt"
         "../ams/machine.rkt"
         "../common/data-structures.rkt"
         "../common/case-hex.rkt"
         "../common/data-utils.rkt"
         "../quick-bpf/machine-instance.rkt"
         "../riscv/machine-instance.rkt")

(require (only-in "../common/debug.rkt" [debugln common:debugln]))

(define DEBUG #f)

(define (debugln x)
  (if DEBUG (void (common:debugln x)) void))

(provide make-stp)

(define (make-stp
          [ebpf-instrs #f]
          [riscv-instrs #f])
  (debugln "Loading BPF machine")
  (define start-time (current-seconds))
  (define bpf-machine (make-quick-bpf-machine))

  (debugln (~a "Loading BPF machine time: " (- (current-seconds) start-time) "s"))

  (debugln "Loading RISC-V machine")
  (set! start-time (current-seconds))
  (define riscv-machine (make-riscv-machine))

  (debugln (~a "Loading RISC-V machine time: " (- (current-seconds) start-time) "s"))

  (define s2t-reg-index
    (lambda (index)
      (case-hex index 8
                [(0) (bv 15 8)]
                [(1) (bv 10 8)]
                [(2) (bv 11 8)]
                [(3) (bv 12 8)]
                [(4) (bv 13 8)]
                [(5) (bv 14 8)]
                [(6) (bv 9 8)]
                [(7) (bv 18 8)]
                [(8) (bv 19 8)]
                [(9) (bv 20 8)]
                [(10) (bv 21 8)]
                [(11) (bv 5 8)])))

  (define (riscv-regs->bpf regs)
    (make-store-from-existing
      regs s2t-reg-index 12))

  (define (riscv-pc->bpf pc)
    ; TODO logical or arithmetic?
    (bvlshr pc (bv 2 64)))

  ; TODO also make usable-regs
  (define scratch-regs
    (map (bvl 5) (list 6 7 28 29 30 31)))

  (define usable-regs
    (map (bvl 5)
         (list 6 7 28 29 30 31
               15 10 11 12 13 14 9 18 19 20 21 5
               0)))

  (src/tgt
    bpf-machine
    riscv-machine
    scratch-regs
    usable-regs
    s2t-reg-index
    riscv-regs->bpf
    identity ; memory mapping
    riscv-pc->bpf
    4 ; pc fact
    64)) ; parameter size
