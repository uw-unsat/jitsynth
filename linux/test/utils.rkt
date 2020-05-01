#lang rosette

(require "../../common/instructions.rkt"
         "../../common/data-utils.rkt"
         "../../common/data-structures.rkt"
         "../../ams/machine.rkt"
         "../../bpf/constants.rkt")

(provide (all-defined-out))

(define (MAKE_BPF_STMT OP DST SRC OFF IMM)
  (let ([bvop (bv OP 8)])
    (bpf-instr 
      ((if (= OP #x18) bv64 bv32) IMM)
      (bv16 OFF) (bv SRC 4) (bv DST 4)
      (extract 7 4 bvop) (extract 3 3 bvop) (extract 2 0 bvop))))

(define (BPF_ALU64_REG OP DST SRC)
  (MAKE_BPF_STMT (bitwise-ior BPF_ALU64 OP BPF_X) DST SRC 0 0))

(define (BPF_ALU32_REG OP DST SRC)
  (MAKE_BPF_STMT (bitwise-ior BPF_ALU OP BPF_X) DST SRC 0 0))

(define (BPF_ALU64_IMM OP DST IMM)
  (MAKE_BPF_STMT (bitwise-ior BPF_ALU64 OP BPF_K) DST 0 0 IMM))

(define (BPF_ALU32_IMM OP DST IMM)
  (MAKE_BPF_STMT (bitwise-ior BPF_ALU OP BPF_K) DST 0 0 IMM))

#|
/* Endianess conversion, cpu_to_{l,b}e(), {l,b}e_to_cpu() */

#define BPF_ENDIAN(TYPE, DST, LEN)				\
	((struct bpf_insn) {					\
		.code  = BPF_ALU | BPF_END | BPF_SRC(TYPE),	\
		.dst_reg = DST,					\
		.src_reg = 0,					\
		.off   = 0,					\
		.imm   = LEN })
  |#

(define (BPF_MOV64_REG DST SRC)
  (MAKE_BPF_STMT (bitwise-ior BPF_ALU64 BPF_MOV BPF_X) DST SRC 0 0))

(define (BPF_MOV32_REG DST SRC)
  (MAKE_BPF_STMT (bitwise-ior BPF_ALU BPF_MOV BPF_X) DST SRC 0 0))

(define (BPF_MOV64_IMM DST IMM)
  (MAKE_BPF_STMT (bitwise-ior BPF_ALU64 BPF_MOV BPF_K) DST 0 0 IMM))

(define (BPF_MOV32_IMM DST IMM)
  (MAKE_BPF_STMT (bitwise-ior BPF_ALU BPF_MOV BPF_K) DST 0 0 IMM))

(define (BPF_LDX_MEM SIZE DST SRC OFF)
  (MAKE_BPF_STMT (bitwise-ior BPF_LDX SIZE BPF_MEM) DST SRC OFF 0))

(define (BPF_STX_MEM SIZE DST SRC OFF)
  (MAKE_BPF_STMT (bitwise-ior BPF_STX SIZE BPF_MEM) DST SRC OFF 0))

#|
/* Atomic memory add, *(uint *)(dst_reg + off16) += src_reg */

#define BPF_STX_XADD(SIZE, DST, SRC, OFF)			\
	((struct bpf_insn) {					\
		.code  = BPF_STX | BPF_SIZE(SIZE) | BPF_XADD,	\
		.dst_reg = DST,					\
		.src_reg = SRC,					\
		.off   = OFF,					\
		.imm   = 0 })
  |#
; NOTE: This is unsupported, so NOP for now
(define (BPF_STX_XADD SIZE DST SRC OFF)
  (MAKE_BPF_STMT (bitwise-ior BPF_STX SIZE BPF_XADD) DST SRC OFF 0))

(define (BPF_ST_MEM SIZE DST OFF IMM)
  (MAKE_BPF_STMT (bitwise-ior BPF_ST SIZE BPF_MEM) DST 0 OFF IMM))

(define (BPF_JMP_REG OP DST SRC OFF)
  (MAKE_BPF_STMT (bitwise-ior BPF_JMP OP BPF_X) DST SRC OFF 0))

(define (BPF_JMP_IMM OP DST IMM OFF)
  (MAKE_BPF_STMT (bitwise-ior BPF_JMP OP BPF_K) DST 0 OFF IMM))

(define (BPF_LD_IMM64 DST IMM)
  (list
    (MAKE_BPF_STMT #x07 0 0 0 0)
    (MAKE_BPF_STMT #x18 DST 0 0 IMM)))

(define (BPF_EXIT_INSN)
  (MAKE_BPF_STMT (bitwise-ior BPF_JMP BPF_EXIT) 0 0 0 0))





; code is a list of BPF instructions, making a program
; extected is a hash-map of register numers to extected values
(struct linux-test-case (code expected) #:transparent)

; NOTE: Assuming no memory
(define (check-test-case ltc interp)
  (let ([actual (state-regs (interp (linux-test-case-code ltc)))]
        [expected (linux-test-case-expected ltc)])
    (for/and ([reg (hash-keys expected)])
      (bveq (bv64 (hash-ref expected reg))
            (read-register actual (bv8 reg))))))

