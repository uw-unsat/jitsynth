#lang rosette

(require "../../common/data-utils.rkt"
         "../../common/data-structures.rkt"
         "../../common/instructions.rkt"
         "../../riscv/machine-instance.rkt"
         "../../riscv/interpreter.rkt"
         "../../ams/machine.rkt")

(require rackunit)

(require (only-in "../../common/debug.rkt" [debugln common:debugln]))

; TODO make this better? Take stuff out?
(provide (except-out (all-defined-out) DEBUG debugln))

(define DEBUG #f)

(define (debugln x)
  (if DEBUG (common:debugln x) x))

;-----------------------------------------------------------------------
; Helper macros
;-----------------------------------------------------------------------

; We use a macro hack to simpify code generation for various numbers
; of bubble cycles.

(define (TEST_INSERT_NOPS i)
  (if (= i 0)
    null
    (cons riscv-nop (TEST_INSERT_NOPS (sub1 i)))))

(define x0 (bv 0 5))
(define x1 (bv 1 5))
(define x2 (bv 2 5))
(define x3 (bv 3 5))
(define x4 (bv 4 5))
(define x5 (bv 5 5))
(define x6 (bv 6 5))

(define temp1 (bv 7 5))
(define temp2 (bv 8 5))

(define tdat1 (bv16 #x00ff))
(define tdat2 (bv16 #xff00))
(define tdat3 (bv16 #x0ff0))
(define tdat4 (bv16 #xf00f))
(define tdat (concat tdat1 tdat2 tdat3 tdat4))

; TODO should I just actually use sign extend?
(define (SEXT_IMM x)
  ; ((x) | (-(((x) >> 11) & 1) << 11))
  (set! x (if (integer? x) (bv x 12) x))
  (bvor x (bvshl (bvneg (bvand (bvashr x (bv 11 12)) (bv 1 12))) (bv 11 12))))

(define (set-reg st reg val)
  (set! val (if (integer? val) (bv64 val) val))
  (make-state
    (write-register (state-regs st) reg val)
    (state-mem st)
    (state-pc st)))

(define (set-regs-func regs vals)
  (if (empty? regs) identity
    (lambda (st)
      (set-reg
        ((set-regs-func (rest regs) (rest vals)) st)
        (first regs)
        (first vals)))))

(define (li reg val)
  (set! val (if (integer? val) (bv32 val) val))
  (define adj (bvadd val (bvand (bv32 #x00001000) (bvshl val (bv32 #x00000001)))))
  (define upper (extract 31 12 adj))
  (define lower (extract 11 0 adj))
  (list
    (make 'lui reg #:imm upper)
    (make 'addi32 reg x0 #:imm lower)))

#|
(define (la reg val)
  ; TODO
  )
|#

(define (symbol-replace sym curr repl)
  (string->symbol (string-replace (symbol->string sym) curr repl)))

; TODO consider imm as integer or bitvector
; reg1 = dst or src1, reg2 = src1 or src2, reg3 = src2, depending on instruction
(define (make instr-name reg1 [reg2 #f] [reg3 #f] #:imm [imm #f])
  (set! instr-name
    (symbol-replace
      (symbol-replace
        (symbol-replace
          (symbol-replace
            instr-name "w" "32")
          "sll" "shl")
        "sra" "ashr")
      "srl" "lshr"))
  (define type (riscv-constructor instr-name))
  (cond
    [(equal? type riscv-rtype)
     (assert reg2)
     (assert reg3)
     (riscv-rtype
       (param-values 'func7 instr-name)
       reg3
       reg2 
       (param-values 'func3 instr-name)
       reg1
       (param-values 'op instr-name))]
    [(equal? type riscv-utype)
     (assert imm)
     (riscv-utype
       (if (integer? imm) (bv imm 20) imm)
       reg1
       (param-values 'op instr-name))]
    [(equal? type riscv-itype)
     (assert reg2)
     (assert imm)
     (define my-imm
       (bvor
         (case instr-name [(ashri ashri32) (bv #b10000000000 12)] [else (bvn0l 12)])
         (if (integer? imm) (bv imm 12) imm)))
     (riscv-itype
       my-imm
       reg2
       reg1
       (param-values 'func3 instr-name)
       (param-values 'op instr-name))]
    [(equal? type riscv-stype)
     (assert reg2)
     (assert imm)
     (riscv-stype
       (if (integer? imm) (bv imm 12) imm)
       reg2
       reg1
       (param-values 'func3 instr-name)
       (param-values 'op instr-name))]
    [(equal? type riscv-btype)
     ; TODO remove
     ; (displayln (~a "Jump imm: " imm))
     (assert reg2)
     (assert imm)
     (riscv-btype
       (if (integer? imm) (bv imm 12) imm)
       reg2
       reg1
       (param-values 'func3 instr-name)
       (param-values 'op instr-name))]
    [(equal? type riscv-jtype)
     (assert imm)
     (riscv-utype
       (if (integer? imm) (bv imm 20) imm)
       reg1
       (param-values 'op instr-name))]))

(define (TEST_CASE testnum instr reg result prog #:state-update-func [sf identity])
  (debugln (~a "Instruction: " instr))
  (debugln (~a "Test case number: " testnum))
  (test-case (~a "Test number " testnum " for instruction " instr)
    (set! result (if (integer? result) (bv64 result) result))
    (define starting-state
      (sf (make-state (make-zerod-registers 32) (make-zerod-memory 128) (bv64 0))))
    (define actual-result
      (read-register
        ; TODO make the bound less stupid?
        (state-regs
          (interpret-with-state
            prog
            (state-regs starting-state)
            (state-mem starting-state)
            (state-pc starting-state)
            (bv64 (* 10 (length prog)))))
        reg))
    ; TODO need something special for bveq?
    (check-equal? actual-result result)))

;-----------------------------------------------------------------------
; RV64UI MACROS
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
; Tests for instructions with immediate operand
;-----------------------------------------------------------------------

(define (SRL_VAL v a)
  (set! v (if (integer? v) (bv64 v) v))
  (set! a (if (integer? a) (bv64 a) a))
  (bvlshr v a))

(define (TEST_SRL n v a)
  (TEST_IMM_OP n 'srli (SRL_VAL v a) v a))

(define (TEST_SRLI n v a)
  (TEST_RR_OP n 'srl (SRL_VAL v a) v a))

(define (TEST_IMM_OP testnum inst result val1 imm)
  (TEST_CASE
    testnum inst x3 result
    #:state-update-func (set-regs-func (list x1) (list val1))
    (flatten-list
      (make inst x3 x1 #:imm (SEXT_IMM imm)))))

(define (TEST_IMM_SRC1_EQ_DEST testnum inst result val1 imm)
  (TEST_CASE
    testnum inst x1 result
    #:state-update-func (set-regs-func (list x1) (list val1))
    (flatten-list
      (make inst x1 x1 #:imm (SEXT_IMM imm)))))

(define (TEST_IMM_DEST_BYPASS testnum nop_cycles inst result val1 imm)
  (define midcomp
    (flatten-list
      (make inst x3 x1 #:imm (SEXT_IMM imm))
      (TEST_INSERT_NOPS nop_cycles)
      (make 'addi x6 x3 #:imm 0)
      (make 'addi x4 x4 #:imm 1)
      (li x5 2)))
  (TEST_CASE
    testnum inst x6 result
    #:state-update-func (set-regs-func (list x1) (list val1))
    (flatten-list
      (li x4 0)
      midcomp
      (make 'bne x4 x5 #:imm (- (* 2 (length midcomp)))))))

(define (TEST_IMM_SRC1_BYPASS testnum nop_cycles inst result val1 imm)
  (define midcomp
    (flatten-list
      (TEST_INSERT_NOPS nop_cycles)
      (make inst x3 x1 #:imm (SEXT_IMM imm))
      (make 'addi x4 x4 #:imm 1)
      (li x5 2)))
  (TEST_CASE
    testnum inst x3 result
    #:state-update-func (set-regs-func (list x1) (list val1))
    (flatten-list
      (li x4 0)
      midcomp
      (make 'bne x4 x5 #:imm (- (* 2 (length midcomp)))))))

(define (TEST_IMM_ZEROSRC1 testnum inst result imm)
  (TEST_CASE
    testnum inst x1 result
    (list (make inst x1 x0 #:imm (SEXT_IMM imm)))))

(define (TEST_IMM_ZERODEST testnum inst val1 imm)
  (TEST_CASE
    testnum inst x0 0
    #:state-update-func (set-regs-func (list x1) (list val1))
    (flatten-list
      (make inst x0 x1 #:imm (SEXT_IMM imm)))))

;-----------------------------------------------------------------------
; Tests for an instruction with register operands
;-----------------------------------------------------------------------

(define (TEST_R_OP testnum inst result val1)
  (TEST_CASE
    testnum inst x3 result
    #:state-update-func (set-regs-func (list x1) (list val1))
    (flatten-list
      (make inst x3 x1))))

(define (TEST_R_SRC1_EQ_DEST testnum inst result val1)
  (TEST_CASE
    testnum inst x1 result
    #:state-update-func (set-regs-func (list x1) (list val1))
    (flatten-list
      (make inst x1 x1))))

(define (TEST_R_DEST_BYPASS testnum nop_cycles inst result val1)
  (define midcomp
    (flatten-list
      (make inst x3 x1)
      (TEST_INSERT_NOPS nop_cycles)
      (make 'addi x6 x3 #:imm 0)
      (make 'addi  x4 x4 #:imm 1)
      (li x5 2)))
  (TEST_CASE
    testnum inst x6 result
    #:state-update-func (set-regs-func (list x1) (list val1))
    (flatten-list
      (li x4 0)
      midcomp
      (make 'bne x4 x5 #:imm (- (* 2 (length midcomp)))))))

;-----------------------------------------------------------------------
; Tests for an instruction with register-register operands
;-----------------------------------------------------------------------

(define (TEST_RR_OP testnum inst result val1 val2)
  (TEST_CASE
    testnum inst x3 result
    #:state-update-func (set-regs-func (list x1 x2) (list val1 val2))
    (flatten-list
      (make inst x3 x1 x2))))

(define (TEST_RR_SRC1_EQ_DEST testnum inst result val1 val2)
  (TEST_CASE
    testnum inst x1 result
    #:state-update-func (set-regs-func (list x1 x2) (list val1 val2))
    (flatten-list
      (make inst x1 x1 x2))))

(define (TEST_RR_SRC2_EQ_DEST testnum inst result val1 val2)
  (TEST_CASE
    testnum inst x2 result
    #:state-update-func (set-regs-func (list x1 x2) (list val1 val2))
    (flatten-list
      (make inst x2 x1 x2))))

(define (TEST_RR_SRC12_EQ_DEST testnum inst result val1)
  (TEST_CASE
    testnum inst x1 result
    #:state-update-func (set-regs-func (list x1) (list val1))
    (flatten-list
      (make inst x1 x1 x1))))

(define (TEST_RR_DEST_BYPASS testnum nop_cycles inst result val1 val2)
  (define midcomp
    (flatten-list
      (make inst x3 x1 x2)
      (TEST_INSERT_NOPS nop_cycles)
      (make 'addi x6 x3 #:imm 0)
      (make 'addi x4 x4 #:imm 1)
      (li x5 2)))
  (TEST_CASE
    testnum inst x6 result
    #:state-update-func (set-regs-func (list x1 x2) (list val1 val2))
    (flatten-list
      (li x4 0)
      midcomp
      (make 'bne x4 x5 #:imm (- (* 2 (length midcomp)))))))

(define (TEST_RR_SRC12_BYPASS testnum src1_nops src2_nops inst result val1 val2)
  (define midcomp
    (flatten-list
      (TEST_INSERT_NOPS src1_nops)
      (TEST_INSERT_NOPS src2_nops)
      (make inst x3 x1 x2)
      (make 'addi x4 x4 #:imm 1)
      (li x5 2)))
  (TEST_CASE
    testnum inst x3 result
    #:state-update-func (set-regs-func (list x1 x2) (list val1 val2))
    (flatten-list
      (li x4 0)
      midcomp
      (make 'bne x4 x5 #:imm (- (* 2 (length midcomp)))))))

(define (TEST_RR_SRC21_BYPASS testnum src1_nops src2_nops inst result val1 val2)
  (define midcomp
    (flatten-list
      (TEST_INSERT_NOPS src1_nops)
      (TEST_INSERT_NOPS src2_nops)
      (make inst x3 x1 x2)
      (make 'addi x4 x4 #:imm 1)
      (li x5 2)))
  (TEST_CASE
    testnum inst x3 result
    #:state-update-func (set-regs-func (list x1 x2) (list val1 val2))
    (flatten-list
      (li x4 0)
      midcomp
      (make 'bne x4 x5 #:imm (- (* 2 (length midcomp)))))))

(define (TEST_RR_ZEROSRC1 testnum inst result val)
  (TEST_CASE
    testnum inst x2 result
    #:state-update-func (set-regs-func (list x1) (list val))
    (flatten-list
      (make inst x2 x0 x1))))

(define (TEST_RR_ZEROSRC2 testnum inst result val)
  (TEST_CASE
    testnum inst x2 result
    #:state-update-func (set-regs-func (list x1) (list val))
    (flatten-list
      (make inst x2 x1 x0))))

(define (TEST_RR_ZEROSRC12 testnum inst result)
  (TEST_CASE
    testnum inst x1 result
    (flatten-list
      (make inst x1 x0 x0))))

(define (TEST_RR_ZERODEST testnum inst val1 val2)
  (TEST_CASE
    testnum inst x0 0
    #:state-update-func (set-regs-func (list x1 x2) (list val1 val2))
    (flatten-list
      (make inst x0 x1 x2))))


;-----------------------------------------------------------------------
; Test memory instructions
;-----------------------------------------------------------------------

#|
; TODO
(define (TEST_LD_OP testnum inst result offset base)
  (TEST_CASE
    testnum inst x3 result
    (flatten-list
      (la x1 base)
      (make inst x3 x1 #:imm offset))))

(define (TEST_ST_OP testnum load_inst store_inst result offset base)
  (TEST_CASE
    testnum inst x3 result
    (flatten-list
      (la x1 base)
      (li x2 result)
      (make store_inst x2 x1 #:imm offset)
      (make load_inst x3 x1 #:imm offset))))

(define (TEST_LD_DEST_BYPASS testnum nop_cycles inst result offset base)
  ; TODO test label?
  (TEST_LABEL(testnum TESTSUITE): \
    li  TESTNUM testnum; \
    li  x4 0; \
1:  la  x1 base; \
    inst x3 offset(x1); \
    TEST_INSERT_NOPS_ ## nop_cycles \
    addi  x6 x3 0; \
    li  x29 result; \
    bne x6 x29 PASSFAIL_LABEL(fail TESTSUITE); \
    addi  x4 x4 1; \
    li  x5 2; \
    bne x4 x5 1b; \

#define TEST_LD_SRC1_BYPASS( testnum nop_cycles inst result offset base ) \
    TEST_LABEL(testnum TESTSUITE): \
    li  TESTNUM testnum; \
    li  x4 0; \
1:  la  x1 base; \
    TEST_INSERT_NOPS_ ## nop_cycles \
    inst x3 offset(x1); \
    li  x29 result; \
    bne x3 x29 PASSFAIL_LABEL(fail TESTSUITE); \
    addi  x4 x4 1; \
    li  x5 2; \
    bne x4 x5 1b \

#define TEST_ST_SRC12_BYPASS( testnum src1_nops src2_nops load_inst store_inst result offset base ) \
    TEST_LABEL(testnum TESTSUITE): \
    li  TESTNUM testnum; \
    li  x4 0; \
1:  la  x1 result; \
    TEST_INSERT_NOPS_ ## src1_nops \
    la  x2 base; \
    TEST_INSERT_NOPS_ ## src2_nops \
    store_inst x1 offset(x2); \
    load_inst x3 offset(x2); \
    li  x29 result; \
    bne x3 x29 PASSFAIL_LABEL(fail TESTSUITE); \
    addi  x4 x4 1; \
    li  x5 2; \
    bne x4 x5 1b \

#define TEST_ST_SRC21_BYPASS( testnum src1_nops src2_nops load_inst store_inst result offset base ) \
    TEST_LABEL(testnum TESTSUITE): \
    li  TESTNUM testnum; \
    li  x4 0; \
1:  la  x2 base; \
    TEST_INSERT_NOPS_ ## src1_nops \
    la  x1 result; \
    TEST_INSERT_NOPS_ ## src2_nops \
    store_inst x1 offset(x2); \
    load_inst x3 offset(x2); \
    li  x29 result; \
    bne x3 x29 PASSFAIL_LABEL(fail TESTSUITE); \
    addi  x4 x4 1; \
    li  x5 2; \
    bne x4 x5 1b \

;-----------------------------------------------------------------------
; Test branch instructions
;-----------------------------------------------------------------------

#define TEST_BR1_OP_TAKEN( testnum inst val1 ) \
    TEST_LABEL(testnum TESTSUITE): \
    li  TESTNUM testnum; \
    li  x1 val1; \
    inst x1 2f; \
    bne x0 TESTNUM PASSFAIL_LABEL(fail TESTSUITE); \
1:  bne x0 TESTNUM 3f; \
2:  inst x1 1b; \
    bne x0 TESTNUM PASSFAIL_LABEL(fail TESTSUITE); \
3:

#define TEST_BR1_OP_NOTTAKEN( testnum inst val1 ) \
    TEST_LABEL(testnum TESTSUITE): \
    li  TESTNUM testnum; \
    li  x1 val1; \
    inst x1 1f; \
    bne x0 TESTNUM 2f; \
1:  bne x0 TESTNUM PASSFAIL_LABEL(fail TESTSUITE); \
2:  inst x1 1b; \
3:

#define TEST_BR1_SRC1_BYPASS( testnum nop_cycles inst val1 ) \
    TEST_LABEL(testnum TESTSUITE): \
    li  TESTNUM testnum; \
    li  x4 0; \
1:  li  x1 val1; \
    TEST_INSERT_NOPS_ ## nop_cycles \
    inst x1 PASSFAIL_LABEL(fail TESTSUITE); \
    addi  x4 x4 1; \
    li  x5 2; \
    bne x4 x5 1b \

#define TEST_BR2_OP_TAKEN( testnum inst val1 val2 ) \
    TEST_LABEL(testnum TESTSUITE): \
    li  TESTNUM testnum; \
    li  x1 val1; \
    li  x2 val2; \
    inst x1 x2 2f; \
    bne x0 TESTNUM PASSFAIL_LABEL(fail TESTSUITE); \
1:  bne x0 TESTNUM 3f; \
2:  inst x1 x2 1b; \
    bne x0 TESTNUM PASSFAIL_LABEL(fail TESTSUITE); \
3:

#define TEST_BR2_OP_NOTTAKEN( testnum inst val1 val2 ) \
    TEST_LABEL(testnum TESTSUITE): \
    li  TESTNUM testnum; \
    li  x1 val1; \
    li  x2 val2; \
    inst x1 x2 1f; \
    bne x0 TESTNUM 2f; \
1:  bne x0 TESTNUM PASSFAIL_LABEL(fail TESTSUITE); \
2:  inst x1 x2 1b; \
3:

#define TEST_BR2_SRC12_BYPASS( testnum src1_nops src2_nops inst val1 val2 ) \
    TEST_LABEL(testnum TESTSUITE): \
    li  TESTNUM testnum; \
    li  x4 0; \
1:  li  x1 val1; \
    TEST_INSERT_NOPS_ ## src1_nops \
    li  x2 val2; \
    TEST_INSERT_NOPS_ ## src2_nops \
    inst x1 x2 PASSFAIL_LABEL(fail TESTSUITE); \
    addi  x4 x4 1; \
    li  x5 2; \
    bne x4 x5 1b \

#define TEST_BR2_SRC21_BYPASS( testnum src1_nops src2_nops inst val1 val2 ) \
    TEST_LABEL(testnum TESTSUITE): \
    li  TESTNUM testnum; \
    li  x4 0; \
1:  li  x2 val2; \
    TEST_INSERT_NOPS_ ## src1_nops \
    li  x1 val1; \
    TEST_INSERT_NOPS_ ## src2_nops \
    inst x1 x2 PASSFAIL_LABEL(fail TESTSUITE); \
    addi  x4 x4 1; \
    li  x5 2; \
    bne x4 x5 1b \

;-----------------------------------------------------------------------
; Test jump instructions
;-----------------------------------------------------------------------

#define TEST_JR_SRC1_BYPASS( testnum nop_cycles inst ) \
    TEST_LABEL(testnum TESTSUITE): \
    li  TESTNUM testnum; \
    li  x4 0; \
1:  la  x6 2f; \
    TEST_INSERT_NOPS_ ## nop_cycles \
    inst x6; \
    bne x0 TESTNUM PASSFAIL_LABEL(fail TESTSUITE); \
2:  addi  x4 x4 1; \
    li  x5 2; \
    bne x4 x5 1b \

#define TEST_JALR_SRC1_BYPASS( testnum nop_cycles inst ) \
    TEST_LABEL(testnum TESTSUITE): \
    li  TESTNUM testnum; \
    li  x4 0; \
1:  la  x6 2f; \
    TEST_INSERT_NOPS_ ## nop_cycles \
    inst x19 x6 0; \
    bne x0 TESTNUM PASSFAIL_LABEL(fail TESTSUITE); \
2:  addi  x4 x4 1; \
    li  x5 2; \
    bne x4 x5 1b \
|#
