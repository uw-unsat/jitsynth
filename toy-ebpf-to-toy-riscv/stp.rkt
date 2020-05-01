#lang rosette

(require "../ams/machine.rkt"
         "../common/case-hex.rkt"
         "../common/data-utils.rkt"
         "../common/data-structures.rkt"
         "../toy-ebpf/machine-instance.rkt"
         "../toy-riscv/machine-instance.rkt")

(provide make-stp)

(define bpf-machine
  (make-toy-ebpf-machine)) 

(define riscv-machine
  (make-toy-riscv-machine))

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
  (bvlshr pc (bv 2 64)))

(define scratch-regs
  (map (bvl 5) (list 6 7 28 29 30 31)))

(define usable-regs
  (map (bvl 5)
       (list 6 7 28 29 30 31
             15 10 11 12 13 14 9 18 19 20 21 5
             0)))

(define make-stp
  (thunk*
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
      64))) ; Maximum parameter size
