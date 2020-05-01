#lang rosette

(require "../../gensketch/rwsets.rkt" "../../common/data-utils.rkt"
         "../../ams/machine-loader.rkt" "../../common/instructions.rkt")

(require (only-in "../../riscv/interpreter.rkt" [interpret-with-state riscv:interpret-with-state]))
(require (only-in "../../bpf/interpreter.rkt" [interpret-with-state bpf:interpret-with-state]))

(require rackunit)

(provide riscv-write-set-tests riscv-read-set-tests
         bpf-write-set-tests bpf-read-set-tests)

(define rvregbits 5)
(define rvregbitlen 64)
(define membits 7)

(define (make-riscv-itype-instruction name opcode)
  (make-instruction name riscv-itype (make-list 5 #f)
                    (flatten-list (map symbv-thunk (list 12 5 5 3)) (thunk opcode))
                    (list 'value 'reg 'reg 'op 'op)
                    (list 12 5 5 3 7)))

(define (make-riscv-rtype-instruction name opcode)
  (make-instruction name riscv-rtype (make-list 6 #f)
                    (flatten-list (map symbv-thunk (list 7 5 5 3 5)) (thunk opcode))
                    (list 'value 'reg 'reg 'op 'reg 'op)
                    (list 7 5 5 3 5 7)))

(define (make-riscv-stype-instruction name opcode)
  (make-instruction name riscv-stype (make-list 5 #f)
                    (flatten-list (map symbv-thunk (list 12 5 5 3)) (thunk opcode))
                    (list 'value 'reg 'reg 'op 'op)
                    (list 12 5 5 3 7)))

(define (make-riscv-btype-instruction name opcode)
  (make-instruction name riscv-btype (make-list 5 #f)
                    (flatten-list (map symbv-thunk (list 12 5 5 3)) (thunk opcode))
                    (list 'value 'reg 'reg 'op 'op)
                    (list 12 5 5 3 7)))

(define (make-riscv-utype-instruction name opcode)
  (make-instruction name riscv-utype (make-list 3 #f)
                    (flatten-list (map symbv-thunk (list 20 5)) (thunk opcode))
                    (list 'value 'reg 'op)
                    (list 20 5 7)))

(define (rv-set name opcode instr-type read?)
  ((if read? read-set write-set)
    ((case instr-type
       [(itype) make-riscv-itype-instruction]
       [(stype) make-riscv-stype-instruction]
       [(btype) make-riscv-btype-instruction]
       [(rtype) make-riscv-rtype-instruction]
       [(utype) make-riscv-utype-instruction])
     name opcode)
    riscv-nop rvregbits rvregbitlen membits riscv:interpret-with-state))

(define (rv-read-set name opcode instr-type)
  (rv-set name opcode instr-type #t))

(define (rv-write-set name opcode instr-type)
  (rv-set name opcode instr-type #f))

; RISC-V WRITE SETS
(define-test-suite riscv-write-set-tests
  ; Immediate Arith instr
  (check-set-equal?
    (rv-write-set 'rvia (bv #x13 7) 'itype)
    '(regs))

  ; Register Arith instr
  (check-set-equal?
    (rv-write-set 'rvra (bv #x33 7) 'rtype)
    '(regs))

  ; Load instr
  (check-set-equal?
    (rv-write-set 'rvl (bv #x03 7) 'itype)
    '(regs))

  ; Store instr
  (check-set-equal?
    (rv-write-set 'rvs (bv #x23 7) 'stype)
    '(mem))

  ; Branch instr
  (check-set-equal?
    (rv-write-set 'rvb (bv #x63 7) 'btype)
    '(pc))

  ; JALR instr
  (check-set-equal?
    (rv-write-set 'rvjalr (bv #x67 7) 'itype)
    '(regs pc))

  ; AUIPC instr
  (check-set-equal?
    (rv-write-set 'rvauipc (bv #x17 7) 'utype)
    '(regs)))

(define-test-suite riscv-read-set-tests
  ; RISC-V READ SETS
  ; Immediate Arith instr
  (check-set-equal?
    (rv-read-set 'rvia (bv #x13 7) 'itype)
    '(regs))

  ; Register Arith instr
  (check-set-equal?
    (rv-read-set 'rvra (bv #x33 7) 'rtype)
    '(regs))

  ; Load instr
  (check-set-equal?
    (rv-read-set 'rvl (bv #x03 7) 'itype)
    '(mem regs))

  ; Store instr
  (check-set-equal?
    (rv-read-set 'rvs (bv #x23 7) 'stype)
    '(mem regs))

  ; Branch instr
  (check-set-equal?
    (rv-read-set 'rvb (bv #x63 7) 'btype)
    '(pc regs))

  ; JALR instr
  (check-set-equal?
    (rv-read-set 'rvjalr (bv #x67 7) 'itype)
    '(regs))

  ; AUIPC instr
  (check-set-equal?
    (rv-read-set 'rvauipc (bv #x17 7) 'utype)
    '(pc regs)))

; BPF STUFF

(define bpfregbits 4)
(define bpfregbitlen 64)
; TODO membits

(define bpf-nop
  (apply bpf-instr
         (flatten-list
           (map bvn0l (list 32 16 4 4 4 1))
           (bv 7 3))))

(define (lists->bpf-instr imm off src dst oplist)
  (apply bpf-instr (flatten-list imm off src dst oplist)))

(define (bpf-set name opcode read?)
  ((if read? read-set write-set)
   (make-instruction
     name
     lists->bpf-instr
     (make-list 5 #f)
     (flatten-list
       (thunk (symbv 32))
       (thunk (symbv 16))
       (thunk (symbv 4))
       (thunk (symbv 4))
       (thunk opcode))
     (list 'value 'value 'reg 'reg 'op)
     (list 32 16 4 4 8))
  bpf-nop bpfregbits bpfregbitlen membits bpf:interpret-with-state))

(define (bpf-read-set name opcode)
  (bpf-set name opcode #t))

(define (bpf-write-set name opcode)
  (bpf-set name opcode #f))

; BPF WRITE SETS
(define-test-suite bpf-write-set-tests
  ; Imm add
  (check-set-equal?
    (bpf-write-set 'bpfaddi (list (bv 0 4) (bv 0 1) (bv 7 3)))
    '(regs))

  ; Reg add
  (check-set-equal?
    (bpf-write-set 'bpfadd (list (bv 0 4) (bv 1 1) (bv 7 3)))
    '(regs))

  ; Imm jump 
  (check-set-equal?
    (bpf-write-set 'bpfjumpi (list (bv #xb 4) (bv 0 1) (bv 5 3)))
    '(pc))

  ; Reg jump 
  (check-set-equal?
    (bpf-write-set 'bpfjump (list (bv #xb 4) (bv 1 1) (bv 5 3)))
    '(pc))

  ; Load
  (check-set-equal?
    (bpf-write-set 'bpfld (list (bv 6 4) (bv 0 1) (bv 1 3)))
    '(regs))

  ; Imm store
  (check-set-equal?
    (bpf-write-set 'bpfsti (list (bv 6 4) (bv 0 1) (bv 2 3)))
    '(mem))

  ; Reg store
  (check-set-equal?
    (bpf-write-set 'bpfst (list (bv 6 4) (bv 0 1) (bv 3 3)))
    '(mem)))

; BPF READ SETS
(define-test-suite bpf-read-set-tests
  ; Imm add
  (check-set-equal?
    (bpf-read-set 'bpfaddi (list (bv 0 4) (bv 0 1) (bv 7 3)))
    '(regs))

  ; Reg add
  (check-set-equal?
    (bpf-read-set 'bpfadd (list (bv 0 4) (bv 1 1) (bv 7 3)))
    '(regs))

  ; Imm jump 
  (check-set-equal?
    (bpf-read-set 'bpfjumpi (list (bv #xb 4) (bv 0 1) (bv 5 3)))
    '(regs pc))

  ; Reg jump 
  (check-set-equal?
    (bpf-read-set 'bpfjump (list (bv #xb 4) (bv 1 1) (bv 5 3)))
    '(regs pc))

  ; Load
  (check-set-equal?
    (bpf-read-set 'bpfld (list (bv 6 4) (bv 0 1) (bv 1 3)))
    '(regs mem))

  ; Imm store
  (check-set-equal?
    (bpf-read-set 'bpfsti (list (bv 6 4) (bv 0 1) (bv 2 3)))
    '(regs mem))

  ; Reg store
  (check-set-equal?
    (bpf-read-set 'bpfst (list (bv 6 4) (bv 0 1) (bv 3 3)))
    '(regs mem)))
