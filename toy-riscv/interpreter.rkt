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

(provide (struct-out rv-insn)
         interpret)

(struct rv-insn
  (opcode rd rs1 rs2 imm))

(define (interpret insn st)
  (case (rv-insn-opcode insn)
    [(lui)
     ; load a 20-bit value into 
     ; upper bits of a register
     (make-state
       (write-register
         (state-regs st)
         (rv-insn-rd insn)
         (sext64
           (concat
             (rv-insn-imm insn)
             (zeros 12))))
       (state-mem st)
       (bvadd (bv64 4) (state-pc st)))]
    [(addiw)
     ; Add 12-bit value to a reg;
     ; sign extend lower 32-bits
     (make-state
       (write-register
         (state-regs st)
         (rv-insn-rd insn)
         (sext64
           (bvadd
             (sext32 (rv-insn-imm insn))
             (extract 31 0
               (read-register
                 (state-regs st)
                 (rv-insn-rs1 insn))))))
       (state-mem st)
       (bvadd (bv64 4) (state-pc st)))]
    [(add)
     ; Add two registers
     (make-state
       (write-register
         (state-regs st)
         (rv-insn-rd insn)
         (bvadd
           (read-register
             (state-regs st)
             (rv-insn-rs1 insn))
           (read-register
             (state-regs st)
             (rv-insn-rs2 insn))))
       (state-mem st)
       (bvadd (bv64 4) (state-pc st)))]
    [(slli)
     ; Left shift reg by value
     (make-state
       (write-register
         (state-regs st)
         (rv-insn-rd insn)
         (bvshl
           (read-register
             (state-regs st)
             (rv-insn-rs1 insn))
           (zext64
             (bvand (bv #x3f 12)
                    (rv-insn-imm insn)))))
       (state-mem st)
       (bvadd (bv64 4) (state-pc st)))]
    [(srli)
     ; Logical right shift
     (make-state
       (write-register
         (state-regs st)
         (rv-insn-rd insn)
         (bvlshr
           (read-register
             (state-regs st)
             (rv-insn-rs1 insn))
           (zext64
             (bvand (bv #x3f 12)
                    (rv-insn-imm insn)))))
       (state-mem st)
       (bvadd (bv64 4) (state-pc st)))]
    [(ld)
     ; Load word from memory
     (make-state
       (write-register
         (state-regs st)
         (rv-insn-rd insn)
         (read-memory
           (state-mem st)
           (bvadd
             (sext64 (rv-insn-imm insn))
             (read-register
               (state-regs st)
               (rv-insn-rs1 insn)))
           (bv8 8)))
       (state-mem st)
       (bvadd (bv64 4) (state-pc st)))]
    [(sd)
     ; Store word to memory
     (make-state
       (state-regs st)
       (write-memory
         (state-mem st)
         (bvadd
           (sext64 (rv-insn-imm insn))
           (read-register
             (state-regs st)
             (rv-insn-rs1 insn)))
         (bv8 8)
         (read-register 
           (state-regs st)
           (rv-insn-rs2 insn)))
       (bvadd (bv64 4) (state-pc st)))]))
