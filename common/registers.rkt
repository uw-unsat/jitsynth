#lang rosette
(provide bpf-reg->riscv)
(provide bpf-reg->riscv-con)
(require "case-hex.rkt")

(define (bpf-reg->riscv-con reg-index)
  (concat (bpf-reg->riscv reg-index #t) (bpf-reg->riscv reg-index #f)))

(define (bpf-reg->riscv reg-index 4-1?)
  (if 4-1?
    (case-hex reg-index 4
      [(0) (bv 0 4)]
      [(1) (bv 6 4)]
      [(2) (bv 6 4)]
      [(3) (bv 7 4)]
      [(4) (bv 7 4)]
      [(5) (bv 8 4)]
      [(6) (bv 9 4)]
      [(7) (bv 9 4)]
      [(8) (bv 10 4)]
      [(9) (bv 10 4)]
      [(10) (bv 1 4)])
    (case-hex reg-index 4
      [(0) (bv 1 1)]
      [(1) (bv 12 1)]
      [(2) (bv 13 1)]
      [(3) (bv 14 1)]
      [(4) (bv 15 1)]
      [(5) (bv 16 1)]
      [(6) (bv 18 1)]
      [(7) (bv 19 1)]
      [(8) (bv 20 1)]
      [(9) (bv 21 1)]
      [(10) (bv 2 1)])))


