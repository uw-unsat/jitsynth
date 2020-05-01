#lang rosette

(require "../ams/machine.rkt"
         "../ams/machine-loader.rkt"
         "../common/data-utils.rkt"
         "../common/data-structures.rkt"
         "interpreter.rkt")

(provide make-toy-ebpf-machine)

(define (make-toy-ebpf-machine)
  (define params-list '(opcode dst src off imm))

  (define bpf-instructions
    (list
      (make-instruction
        'addi32
        ebpf-insn
        (list
          ebpf-insn-opcode
          ebpf-insn-dst
          ebpf-insn-src
          ebpf-insn-off
          ebpf-insn-imm)
        (list
          (thunk 'addi32)
          (symbv-thunk 4)
          (symbv-thunk 4)
          (symbv-thunk 16)
          (symbv-thunk 32))
        '(op reg reg value value)
        (list -1 4 4 16 32))))

  (define bpf-regs (make-registers 12))
  (define bpf-mem (make-indef-memory))
  (define bpf-pc (symbv 64))

  (define (interpret-with-state
            program regs mem pc 
            [max-depth (bv64 -1)])
    (interpret-with-state-helper
      program
      (make-state regs mem pc)
      max-depth))

  (define (interpret-with-state-helper
            program st max-depth)
    (if (or (bvuge (bv64 0) max-depth)
            (empty? program))
      st
      (interpret-with-state-helper
        (rest program)
        (interpret (first program) st)
        (bvsub max-depth (bv64 1)))))

  (make-machine
    ; Generic state
    (make-state bpf-regs bpf-mem bpf-pc)
    ; Function that interprets a program and state
    interpret-with-state
    ; List of instructions
    bpf-instructions
    ; NOP instruction
    (ebpf-insn 'addi32 (bv 0 4) (bv 0 4) (bv 0 16) (bv 0 32))))
