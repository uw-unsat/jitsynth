#lang rosette

(require "../common/case-hex.rkt")
(require "../common/instructions.rkt")
(require (only-in "../common/debug.rkt" [debugln common:debugln]))

(provide tostr-instr)
(provide tostr-instrs)

(define DEBUG #f)

(define (debugln x)
  (if DEBUG (common:debugln x) void))

(define (tostr-func func3 func7)
  (case-hex func3 3
    [(0) (case-hex func7 7
           [(0) "add"]
           [(32) "sub"])]
    [(1) "sll"]
    [(2) (error "slt not implemented")]
    [(3) (error "sltu not implemented")]
    [(4) "xor"] ; NOTE: May need another case for signed division
    [(5) (case-hex func7 7
           [(0) "srl"]
           [(1) "div"]
           [(32) "sra"])] ; arith right shift if func7 is 32
    [(6) "or"]
    [(7) (case-hex func7 7
           [(0) "and"]
           [(1) "mod"])]))

(define (tostr-imm-func func3 imm)
  (case-hex func3 3
    [(0) "addi"]
    [(1) "slli"]
    [(2) (error "slti not implemented")]
    [(3) (error "sltiu not implemented")]
    [(4) "xori"]
    [(5) (case-hex (extract 10 10 imm) 1
           [(0) "srli"]
           [(1) "srai"])]
    [(6) "ori"]
    [(7) "andi"]))
 
(define (tostr-reg x)
  (case-hex x 5
    [(0) "0"]
    [(1) "ra"]
    [(2) "sp"]
    [(3) "gp"]
    [(4) "tp"]
    [(5) "t0"]
    [(6) "t1"]
    [(7) "t2"]
    [(8) "fp"]
    [(9) "s1"]
    [(10) "a0"]
    [(11) "a1"]
    [(12) "a2"]
    [(13) "a3"]
    [(14) "a4"]
    [(15) "a5"]
    [(16) "a6"]
    [(17) "a7"]
    [(18) "s2"]
    [(19) "s3"]
    [(20) "s4"]
    [(21) "s5"]
    [(12) "s6"]
    [(23) "s7"]
    [(24) "s8"]
    [(25) "s9"]
    [(26) "s10"]
    [(27) "s11"]
    [(28) "t3"]
    [(29) "t4"]
    [(30) "t5"]
    [(31) "t6"]))

(define (tostr-imm b)
  (~a "0x" (number->string (bitvector->natural b) 16)))

(define (tostr-off b)
  (~a (number->string (bitvector->integer b))))

(define (tostr-arith-64 ri)
  (let* ([func3 (riscv-instr-func3 ri)]
         [func7 (riscv-instr-func7 ri)]
         [src1 (riscv-instr-src1 ri)]
         [src2 (riscv-instr-src2 ri)]
         [dst (riscv-instr-dst ri)])
    (~a (tostr-func func3 func7) " "
        (tostr-reg dst) ", "
        (tostr-reg src1) ", "
        (tostr-reg src2))))

(define (tostr-arith-32 ri)
  (let* ([func3 (riscv-instr-func3 ri)]
         [func7 (riscv-instr-func7 ri)]
         [src1 (riscv-instr-src1 ri)]
         [src2 (riscv-instr-src2 ri)]
         [dst (riscv-instr-dst ri)]
         [func (~a (tostr-func func3 func7) "w")])
    (~a func " "
        (tostr-reg dst) ", "
        (tostr-reg src1) ", "
        (tostr-reg src2))))

(define (tostr-imm-arith-64 ri)
  (let* ([func3 (riscv-instr-func3 ri)]
         [imm (riscv-instr-imm ri)]
         [src (riscv-instr-src1 ri)]
         [dst (riscv-instr-dst ri)])
    (if (riscv-nop? ri) "nop"
    (~a (tostr-imm-func func3 imm) " "
        (tostr-reg dst) ", "
        (tostr-reg src) ", "
        (tostr-imm imm)))))

(define (tostr-imm-arith-32 ri)
  (let* ([func3 (riscv-instr-func3 ri)]
         [imm (riscv-instr-imm ri)]
         [src (riscv-instr-src1 ri)]
         [dst (riscv-instr-dst ri)]
         [func (~a (tostr-imm-func func3 imm) "w")])
    (~a func " "
        (tostr-reg dst) ", "
        (tostr-reg src) ", "
        (tostr-imm imm))))

(define (tostr-load ri)
  (let* ([width (riscv-instr-func3 ri)]
         [base (riscv-instr-src1 ri)]
         [dst (riscv-instr-dst ri)]
         [offset (riscv-instr-imm ri)]
         [func
           (case-hex width 3
             [(0) "lb"]
             [(1) "lh"]
             [(2) "lw"]
             [(3) "ld"]
             [(4) "lbu"]
             [(5) "lhu"]
             [(6) "lwu"])])
    (~a func " "
        (tostr-reg dst) ", "
        (tostr-off offset) "(" (tostr-reg base) ")")))

(define (tostr-store ri)
  (let* ([width (riscv-instr-func3 ri)]
         [src (riscv-instr-src2 ri)]
         [base (riscv-instr-src1 ri)]
         [offset (riscv-instr-imm ri)]
         [func
           (case-hex width 3
             [(0) "sb"]
             [(1) "sh"]
             [(2) "sw"]
             [(3) "sd"])])
    (~a func " "
        (tostr-off offset) "(" (tostr-reg base) "), "
        (tostr-reg src))))

; TODO semantics here might be wrong, may need to first update PC
(define (tostr-auipc ri)
  (let* ([imm (riscv-instr-imm ri)]
         [dst (riscv-instr-dst ri)])
    (~a "auipc " (tostr-reg dst) ", " (tostr-imm imm))))

; NOTE: The result is sign extended always
(define (tostr-lui ri)
  (let* ([imm (riscv-instr-imm ri)]
         [dst (riscv-instr-dst ri)])
    (~a "lui " (tostr-reg dst) ", " (tostr-imm imm))))

(define (tostr-branch ri)
  (let* ([func3 (riscv-instr-func3 ri)]
         [rs2 (riscv-instr-src2 ri)]
         [rs1 (riscv-instr-src1 ri)]
         [offset (riscv-instr-imm ri)]
         [func (case-hex func3 3
                 [(0) "beq"]
                 [(1) "bne"]
                 [(4) "blt"]
                 [(5) "bge"]
                 [(6) "bltu"]
                 [(7) "bgeu"])])
    (~a func " "
        (tostr-reg rs1) ", "
        (tostr-reg rs2) ", "
        (tostr-off offset))))

(define (tostr-jal ri)
  (let* ([dst (riscv-instr-dst ri)] ; assume this is 0
         [offset (riscv-instr-imm ri)])
    (~a "jal " (tostr-reg dst) ", " (tostr-off offset))))

(define (tostr-jalr ri)
  (let* ([dst (riscv-instr-dst ri)]
         [offset (riscv-instr-imm ri)]
         [src1 (riscv-instr-src1 ri)])
    (~a "jalr " (tostr-reg dst) ", " (tostr-off offset) "(" (tostr-reg src1) ")")))

(define (tostr-instr ri)
  (if (not (null? (symbolics ri)))
    (~a ri " (symbolic)")
    (let* ([opcode (riscv-instr-op ri)])
      (case-hex opcode 7
        [(#x3b) (tostr-arith-32 ri)]
        [(#x1b) (tostr-imm-arith-32 ri)]
        [(#x33) (tostr-arith-64 ri)]
        [(#x13) (tostr-imm-arith-64 ri)]
        [(#x03) (tostr-load ri)]
        [(#x23) (tostr-store ri)]
        [(#x6f) (tostr-jal ri)]
        [(#x63) (tostr-branch ri)]
        [(#x37) (tostr-lui ri)]
        [(#x73) "ret"]
        [(#x67) (tostr-jalr ri)]
        [(#x17) (tostr-auipc ri)]
        [(0) "0"]))))

(define (tostr-instrs ri-list)
  (if (null? ri-list) ""
    (~a (tostr-instr (first ri-list)) "\n" (tostr-instrs (rest ri-list)))))
