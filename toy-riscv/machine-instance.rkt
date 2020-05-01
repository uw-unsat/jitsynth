#lang rosette

(require "../ams/machine.rkt"
         "../ams/machine-loader.rkt"
         "../common/data-utils.rkt"
         "../common/data-structures.rkt"
         "interpreter.rkt")

(provide make-toy-riscv-machine)

(define (make-toy-riscv-machine)
  (define instr-getters
    (list rv-insn-opcode
          rv-insn-rd
          rv-insn-rs1
          rv-insn-rs2
          rv-insn-imm))

  (define (imm-bitlen name)
    (case name
      [(lui) 20]
      [else 12]))

  (define riscv-instructions
    (for/list ([name '(lui addiw add slli srli ld sd)])
      (make-instruction
        name
        rv-insn
        instr-getters
        (list
          (thunk name)
          (symbv-thunk 5)
          (symbv-thunk 5)
          (symbv-thunk 5)
          (symbv-thunk (imm-bitlen name)))
        '(op reg reg reg value)
        (list -1 5 5 5 (imm-bitlen name)))))


  (define riscv-regs (make-registers 32))
  (define riscv-mem (make-indef-memory))
  (define riscv-pc (symbv 64))

  (define (interpret-with-state
            program regs mem pc 
            [max-depth (bv64 -1)])
    (interpret-with-state-helper
      program
      (make-state regs mem pc)
      max-depth))

  (define (interpret-with-state-helper
            program st max-depth)
    ; (when (not (empty? program)) (displayln (~a "rv-iws: " (first program))))
    (if (or (bvuge (bv64 0) max-depth)
            (empty? program))
      st
      (interpret-with-state-helper
        (rest program)
        (interpret (first program) st)
        (bvsub max-depth (bv64 1)))))

  (define start-state
    (make-state
      (write-register
        riscv-regs
        (bv 0 5)
        (bv 0 64))
      riscv-mem
      riscv-pc))

  (make-machine
    ; Generic state
    start-state
    ; Interpreter function
    interpret-with-state
    ; Description of all instructions
    riscv-instructions
    ; NOP
    (rv-insn 'add (bv 0 5) (bv 0 5) (bv 0 5) (bv 0 12))
    ; instr->getters
    (thunk* instr-getters)))
