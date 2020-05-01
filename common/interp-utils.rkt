#lang rosette

(require "data-structures.rkt")
(require "case-hex.rkt")
(require "../riscv/interpreter.rkt")

(provide assemble-and-run)

; TODO remove this
(define (bpf-regs->riscv regs)
  (make-regs-from-existing
   regs
   (lambda (index)
     (if (or (and (bvule (bv 0 8) index) (bvsle index (bv 2 8)))
             (and (bvule (bv 12 8) index) (bvsle index (bv 21 8))))
       (case-hex index 8
                 [(0) (bv 11 8)]
                 [(1) (bv 0 8)]
                 [(2) (bv 10 8)]
                 [(12) (bv 1 8)]
                 [(13) (bv 2 8)]
                 [(14) (bv 3 8)]
                 [(15) (bv 4 8)]
                 [(16) (bv 5 8)]
                 [(16) (bv 12 8)]
                 [(18) (bv 6 8)]
                 [(19) (bv 7 8)]
                 [(20) (bv 8 8)]
                 [(21) (bv 9 8)])
       (if (and (bvule (bv 3 8) index) (bvsle index (bv 11 8)))
         (bvadd index (bv 10 8))
         index)))
   32))

(define (assemble-and-run assembler bpf-instrs regs mem)
  (let* ([riscv-instrs (assembler bpf-instrs)]) 
    (interpret-with-state riscv-instrs (bpf-regs->riscv regs) mem)))
