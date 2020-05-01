#lang rosette

(require
  ; (struct state ...)
  ;   with regs, mem, and pc
  ;   includes state getters
  "../ams/machine.rkt"

  ; Includes functions for manipulating
  ; registers and memory
  "../common/data-structures.rkt"
  
  ; Includes utility functions for bitvectors
  "../common/data-utils.rkt")

(provide (struct-out ebpf-insn)
         interpret)

(struct ebpf-insn
  (opcode dst src off imm)
  #:transparent)

(define (interpret insn st)
  (case (ebpf-insn-opcode insn)
    [(addi32)
     (make-state
       (write-register
         (state-regs st)
         (ebpf-insn-dst insn)
         (zext64
           (bvadd
             (extract 31 0
               (read-register
                 (state-regs st)
                 (ebpf-insn-dst insn)))
               (ebpf-insn-imm insn))))
       (state-mem st)
       (bvadd (bv64 1) (state-pc st)))]))
