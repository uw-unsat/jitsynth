// SPDX-License-Identifier: GPL-2.0
/* BPF JIT compiler for RV64G
 *
 * Copyright(c) 2019 Björn Töpel <bjorn.topel@gmail.com>
 *
 */

// TODO if something is unused, want to replace it with something of the same size probs?

#include <stdbool.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
// #include <linux/bpf.h>
// #include <linux/filter.h>
// #include <asm/cacheflush.h>

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;

typedef enum {
	GFP_KERNEL,
	GFP_ATOMIC,
	__GFP_HIGHMEM,
	__GFP_HIGH
} gfp_t;

static int debug = 0;

static int raw = 1;

// Make this > 1 for bpf_jit_dump
#define bpf_jit_enable 2

/* unused opcode to mark special call to bpf_tail_call() helper */
#define BPF_TAIL_CALL	0xf0

#define MAX_TAIL_CALL_CNT 32

#define BPF_TAG_SIZE	8

/* instruction classes */
#define BPF_JMP32	0x06	/* jmp mode in word width */
#define BPF_ALU64	0x07	/* alu mode in double word width */

/* ld/ldx fields */
#define BPF_DW		0x18	/* double word (64-bit) */
#define BPF_XADD	0xc0	/* exclusive add */

/* alu/jmp fields */
#define BPF_MOV		0xb0	/* mov reg to reg */
#define BPF_ARSH	0xc0	/* sign extending arithmetic shift right */

/* change endianness of a register */
#define BPF_END		0xd0	/* flags for endianness conversion: */
#define BPF_TO_LE	0x00	/* convert to little-endian */
#define BPF_TO_BE	0x08	/* convert to big-endian */
#define BPF_FROM_LE	BPF_TO_LE
#define BPF_FROM_BE	BPF_TO_BE

/* jmp encodings */
#define BPF_JNE		0x50	/* jump != */
#define BPF_JLT		0xa0	/* LT is unsigned, '<' */
#define BPF_JLE		0xb0	/* LE is unsigned, '<=' */
#define BPF_JSGT	0x60	/* SGT is signed '>', GT in x86 */
#define BPF_JSGE	0x70	/* SGE is signed '>=', GE in x86 */
#define BPF_JSLT	0xc0	/* SLT is signed, '<' */
#define BPF_JSLE	0xd0	/* SLE is signed, '<=' */
#define BPF_CALL	0x80	/* function call */
#define BPF_EXIT	0x90	/* function return */

/* Instruction classes */
#define BPF_CLASS(code) ((code) & 0x07)
#define		BPF_LD		0x00
#define		BPF_LDX		0x01
#define		BPF_ST		0x02
#define		BPF_STX		0x03
#define		BPF_ALU		0x04
#define		BPF_JMP		0x05
#define		BPF_RET		0x06
#define		BPF_MISC        0x07

/* ld/ldx fields */
#define BPF_SIZE(code)  ((code) & 0x18)
#define		BPF_W		0x00 /* 32-bit */
#define		BPF_H		0x08 /* 16-bit */
#define		BPF_B		0x10 /*  8-bit */
/* eBPF		BPF_DW		0x18    64-bit */
#define BPF_MODE(code)  ((code) & 0xe0)
#define		BPF_IMM		0x00
#define		BPF_ABS		0x20
#define		BPF_IND		0x40
#define		BPF_MEM		0x60
#define		BPF_LEN		0x80
#define		BPF_MSH		0xa0

/* alu/jmp fields */
#define BPF_OP(code)    ((code) & 0xf0)
#define		BPF_ADD		0x00
#define		BPF_SUB		0x10
#define		BPF_MUL		0x20
#define		BPF_DIV		0x30
#define		BPF_OR		0x40
#define		BPF_AND		0x50
#define		BPF_LSH		0x60
#define		BPF_RSH		0x70
#define		BPF_NEG		0x80
#define		BPF_MOD		0x90
#define		BPF_XOR		0xa0

#define		BPF_JA		0x00
#define		BPF_JEQ		0x10
#define		BPF_JGT		0x20
#define		BPF_JGE		0x30
#define		BPF_JSET        0x40
#define BPF_SRC(code)   ((code) & 0x08)
#define		BPF_K		0x00
#define		BPF_X		0x08


/* Register numbers */
enum {
	BPF_REG_0 = 0,
	BPF_REG_1,
	BPF_REG_2,
	BPF_REG_3,
	BPF_REG_4,
	BPF_REG_5,
	BPF_REG_6,
	BPF_REG_7,
	BPF_REG_8,
	BPF_REG_9,
	BPF_REG_10,
	__MAX_BPF_REG,
};

/* BPF has 10 general purpose 64-bit registers and stack frame. */
#define MAX_BPF_REG	__MAX_BPF_REG

struct bpf_insn {
	u8	code;		/* opcode */
	u8	dst_reg:4;	/* dest register */
	u8	src_reg:4;	/* source register */
	s16	off;		/* signed offset */
	s32	imm;		/* signed immediate constant */
};

struct bpf_binary_header {
	u32 pages;
	/* Some arches need word alignment for their instructions */
	u8 image[]; // TODO need this?: __aligned(4);
};

struct bpf_prog_aux {
	// atomic_t refcnt;
	u32 used_map_cnt;
	u32 max_ctx_offset;
	u32 max_pkt_offset;
	u32 stack_depth;
	u32 id;
	u32 func_cnt; /* used by non-func prog as the number of func progs */
	u32 func_idx; /* 0 for non-func prog, the index in func array for func prog */
	bool offload_requested;
	struct bpf_prog **func;
	struct rv_jit_data *jit_data; /* JIT specific data. arch dependent */
	// struct latch_tree_node ksym_tnode;
	// struct list_head ksym_lnode;
	// const struct bpf_prog_ops *ops;
	struct bpf_map **used_maps;
	struct bpf_prog *prog;
	// struct user_struct *user;
	u64 load_time; /* ns since boottime */
	// struct bpf_map *cgroup_storage[MAX_BPF_CGROUP_STORAGE_TYPE];
	// char name[BPF_OBJ_NAME_LEN];
#ifdef CONFIG_SECURITY
	void *security;
#endif
	// struct bpf_prog_offload *offload;
	// struct btf *btf;
	// struct bpf_func_info *func_info;
	/* bpf_line_info loaded from userspace.  linfo->insn_off
	 * has the xlated insn offset.
	 * Both the main and sub prog share the same linfo.
	 * The subprog can access its first linfo by
	 * using the linfo_idx.
	 */
	// struct bpf_line_info *linfo;
	/* jited_linfo is the jited addr of the linfo.  It has a
	 * one to one mapping to linfo:
	 * jited_linfo[i] is the jited addr for the linfo[i]->insn_off.
	 * Both the main and sub prog share the same jited_linfo.
	 * The subprog can access its first jited_linfo by
	 * using the linfo_idx.
	 */
	void **jited_linfo;
	u32 func_info_cnt;
	u32 nr_linfo;
	/* subprog can use linfo_idx to access its first linfo and
	 * jited_linfo.
	 * main prog always has linfo_idx == 0
	 */
	u32 linfo_idx;
	union {
		// struct work_struct work;
		// struct rcu_head	rcu;
	};
};

// Commented some fields for ease
struct bpf_prog {
	u16			pages;		/* Number of allocated pages */
	u16			jited:1,	/* Is our filter JIT'ed? */
				jit_requested:1,/* archs need to JIT the prog */
				undo_set_mem:1,	/* Passed set_memory_ro() checkpoint */
				gpl_compatible:1, /* Is filter GPL compatible? */
				cb_access:1,	/* Is control block accessed? */
				dst_needed:1,	/* Do we need dst entry? */
				blinded:1,	/* Was blinded */
				is_func:1,	/* program is a bpf function */
				kprobe_override:1, /* Do we override a kprobe? */
				has_callchain_buf:1; /* callchain buffer allocated? */

	// enum bpf_prog_type	type;		/* Type of BPF program */
	// enum bpf_attach_type	expected_attach_type; /* For some prog types */

	u32			len;		/* Number of filter blocks */
	u32			jited_len;	/* Size of jited insns in bytes */
	u8			tag[BPF_TAG_SIZE];
	struct bpf_prog_aux	*aux;		/* Auxiliary fields */
	// struct sock_fprog_kern	*orig_prog;	/* Original BPF program */
	unsigned int		(*bpf_func)(const void *ctx,
					    const struct bpf_insn *insn);
	/* Instructions for interpreter */
	union {
		// struct sock_filter	insns[0];
		struct bpf_insn		*insnsi; // [0]
	};
};

struct bpf_map {
	/* The first two cachelines with read-mostly members of which some
	 * are also accessed in fast-path (e.g. ops, max_entries).
	 */
	// const struct bpf_map_ops *ops; //  ____cacheline_aligned;
	struct bpf_map *inner_map_meta;
#ifdef CONFIG_SECURITY
	void *security;
#endif
	// enum bpf_map_type map_type;
	u32 key_size;
	u32 value_size;
	u32 max_entries;
	u32 map_flags;
	int spin_lock_off; /* >=0 valid offset, <0 error */
	u32 id;
	int numa_node;
	u32 btf_key_type_id;
	u32 btf_value_type_id;
	// struct btf *btf;
	u32 pages;
	bool unpriv_array;
	/* 51 bytes hole */

	/* The 3rd and 4th cacheline with misc members to avoid false sharing
	 * particularly with refcounting.
	 */
	// struct user_struct *user ____cacheline_aligned;
	// atomic_t refcnt;
	// atomic_t usercnt;
	// struct work_struct work;
	// char name[BPF_OBJ_NAME_LEN];
};

struct bpf_array {
	struct bpf_map map;
	u32 elem_size;
	u32 index_mask;
	/* 'ownership' of prog_array is claimed by the first program that
	 * is going to use this map or by the first program which FD is stored
	 * in the map to make sure that all callers and callees have the same
	 * prog_type and JITed flag
	 */
	// enum bpf_prog_type owner_prog_type;
	bool owner_jited;
	union {
		char value[0]; //  __aligned(8);
		void *ptrs[0]; //  __aligned(8);
		// void __percpu *pptrs[0] __aligned(8);
	};
};

enum {
	RV_REG_ZERO =	0,	/* The constant value 0 */
	RV_REG_RA =	1,	/* Return address */
	RV_REG_SP =	2,	/* Stack pointer */
	RV_REG_GP =	3,	/* Global pointer */
	RV_REG_TP =	4,	/* Thread pointer */
	RV_REG_T0 =	5,	/* Temporaries */
	RV_REG_T1 =	6,
	RV_REG_T2 =	7,
	RV_REG_FP =	8,
	RV_REG_S1 =	9,	/* Saved registers */
	RV_REG_A0 =	10,	/* Function argument/return values */
	RV_REG_A1 =	11,	/* Function arguments */
	RV_REG_A2 =	12,
	RV_REG_A3 =	13,
	RV_REG_A4 =	14,
	RV_REG_A5 =	15,
	RV_REG_A6 =	16,
	RV_REG_A7 =	17,
	RV_REG_S2 =	18,	/* Saved registers */
	RV_REG_S3 =	19,
	RV_REG_S4 =	20,
	RV_REG_S5 =	21,
	RV_REG_S6 =	22,
	RV_REG_S7 =	23,
	RV_REG_S8 =	24,
	RV_REG_S9 =	25,
	RV_REG_S10 =	26,
	RV_REG_S11 =	27,
	RV_REG_T3 =	28,	/* Temporaries */
	RV_REG_T4 =	29,
	RV_REG_T5 =	30,
	RV_REG_T6 =	31,
};

#define RV_REG_TCC RV_REG_A6
#define RV_REG_TCC_SAVED RV_REG_S6 /* Store A6 in S6 if program do calls */

static const int regmap[] = {
	[0] =	RV_REG_A5,
	[1] =	RV_REG_A0,
	[2] =	RV_REG_A1,
	[3] =	RV_REG_A2,
	[4] =	RV_REG_A3,
	[5] =	RV_REG_A4,
	[6] =	RV_REG_S1,
	[7] =	RV_REG_S2,
	[8] =	RV_REG_S3,
	[9] =	RV_REG_S4,
	[10] =	RV_REG_S5,
	[11] =	RV_REG_T0,
};

enum {
	RV_CTX_F_SEEN_TAIL_CALL =	0,
	RV_CTX_F_SEEN_CALL =		RV_REG_RA,
	RV_CTX_F_SEEN_S1 =		RV_REG_S1,
	RV_CTX_F_SEEN_S2 =		RV_REG_S2,
	RV_CTX_F_SEEN_S3 =		RV_REG_S3,
	RV_CTX_F_SEEN_S4 =		RV_REG_S4,
	RV_CTX_F_SEEN_S5 =		RV_REG_S5,
	RV_CTX_F_SEEN_S6 =		RV_REG_S6,
};

struct rv_jit_context {
	struct bpf_prog *prog;
	u32 *insns; /* RV insns */
	int ninsns;
	int epilogue_offset;
	int *offset; /* BPF to RV */
	unsigned long flags;
	int stack_size;
};

struct rv_jit_data {
	struct bpf_binary_header *header;
	u8 *image;
	struct rv_jit_context ctx;
};

bool *regbits;
// what's the point of the flags?
static void __set_bit(u8 reg, unsigned long flags)
{
  regbits[reg] = true;
}

static bool test_bit(u8 reg, unsigned long flags)
{
  return regbits[reg];
}

static void pr_comm(char* str)
{
  if (debug)
    printf(str);
}

static void pr_err(char* str, int args, ...)
{
  if (debug)
    printf(str, args);
}

static u8 bpf_to_rv_reg(int bpf_reg, struct rv_jit_context *ctx)
{
	u8 reg = regmap[bpf_reg];

	switch (reg) {
	case RV_CTX_F_SEEN_S1:
	case RV_CTX_F_SEEN_S2:
	case RV_CTX_F_SEEN_S3:
	case RV_CTX_F_SEEN_S4:
	case RV_CTX_F_SEEN_S5:
	case RV_CTX_F_SEEN_S6:
		__set_bit(reg, &ctx->flags);
	}
	return reg;
};

static bool seen_reg(int reg, struct rv_jit_context *ctx)
{
	switch (reg) {
	case RV_CTX_F_SEEN_CALL:
	case RV_CTX_F_SEEN_S1:
	case RV_CTX_F_SEEN_S2:
	case RV_CTX_F_SEEN_S3:
	case RV_CTX_F_SEEN_S4:
	case RV_CTX_F_SEEN_S5:
	case RV_CTX_F_SEEN_S6:
		return test_bit(reg, &ctx->flags);
	}
	return false;
}

static void mark_call(struct rv_jit_context *ctx)
{
	__set_bit(RV_CTX_F_SEEN_CALL, &ctx->flags);
}

static bool seen_call(struct rv_jit_context *ctx)
{
	return test_bit(RV_CTX_F_SEEN_CALL, &ctx->flags);
}

static void mark_tail_call(struct rv_jit_context *ctx)
{
	__set_bit(RV_CTX_F_SEEN_TAIL_CALL, &ctx->flags);
}

static bool seen_tail_call(struct rv_jit_context *ctx)
{
	return test_bit(RV_CTX_F_SEEN_TAIL_CALL, &ctx->flags);
}

static u8 rv_tail_call_reg(struct rv_jit_context *ctx)
{
	mark_tail_call(ctx);

	if (seen_call(ctx)) {
		__set_bit(RV_CTX_F_SEEN_S6, &ctx->flags);
		return RV_REG_S6;
	}
	return RV_REG_A6;
}

// TODO print out here?
static void emit(const u32 insn, struct rv_jit_context *ctx)
{
  pr_err("Instr: 0x%08X\n", insn);
	if (ctx->insns)
  {
    pr_err("Setting at %d\n", ctx->ninsns);
		ctx->insns[ctx->ninsns] = insn;
  } else {
    pr_comm("Insn not set\n");
  }

	ctx->ninsns++;
  pr_err("Num insns: %d\n", ctx->ninsns);
}

static u32 rv_r_insn(u8 funct7, u8 rs2, u8 rs1, u8 funct3, u8 rd, u8 opcode)
{
	return (funct7 << 25) | (rs2 << 20) | (rs1 << 15) | (funct3 << 12) |
		(rd << 7) | opcode;
}

static u32 rv_i_insn(u16 imm11_0, u8 rs1, u8 funct3, u8 rd, u8 opcode)
{
	return (imm11_0 << 20) | (rs1 << 15) | (funct3 << 12) | (rd << 7) |
		opcode;
}

static u32 rv_s_insn(u16 imm11_0, u8 rs2, u8 rs1, u8 funct3, u8 opcode)
{
	u8 imm11_5 = imm11_0 >> 5, imm4_0 = imm11_0 & 0x1f;

	return (imm11_5 << 25) | (rs2 << 20) | (rs1 << 15) | (funct3 << 12) |
		(imm4_0 << 7) | opcode;
}

static u32 rv_sb_insn(u16 imm12_1, u8 rs2, u8 rs1, u8 funct3, u8 opcode)
{
	u8 imm12 = ((imm12_1 & 0x800) >> 5) | ((imm12_1 & 0x3f0) >> 4);
	u8 imm4_1 = ((imm12_1 & 0xf) << 1) | ((imm12_1 & 0x400) >> 10);

	return (imm12 << 25) | (rs2 << 20) | (rs1 << 15) | (funct3 << 12) |
		(imm4_1 << 7) | opcode;
}

static u32 rv_u_insn(u32 imm31_12, u8 rd, u8 opcode)
{
	return (imm31_12 << 12) | (rd << 7) | opcode;
}

static u32 rv_uj_insn(u32 imm20_1, u8 rd, u8 opcode)
{
	u32 imm;

	imm = (imm20_1 & 0x80000) |  ((imm20_1 & 0x3ff) << 9) |
	      ((imm20_1 & 0x400) >> 2) | ((imm20_1 & 0x7f800) >> 11);

	return (imm << 12) | (rd << 7) | opcode;
}

static u32 rv_amo_insn(u8 funct5, u8 aq, u8 rl, u8 rs2, u8 rs1,
		       u8 funct3, u8 rd, u8 opcode)
{
	u8 funct7 = (funct5 << 2) | (aq << 1) | rl;

	return rv_r_insn(funct7, rs2, rs1, funct3, rd, opcode);
}

static u32 rv_addiw(u8 rd, u8 rs1, u16 imm11_0)
{
	return rv_i_insn(imm11_0, rs1, 0, rd, 0x1b);
}

static u32 rv_addi(u8 rd, u8 rs1, u16 imm11_0)
{
	return rv_i_insn(imm11_0, rs1, 0, rd, 0x13);
}

static u32 rv_addw(u8 rd, u8 rs1, u8 rs2)
{
	return rv_r_insn(0, rs2, rs1, 0, rd, 0x3b);
}

static u32 rv_add(u8 rd, u8 rs1, u8 rs2)
{
	return rv_r_insn(0, rs2, rs1, 0, rd, 0x33);
}

static u32 rv_subw(u8 rd, u8 rs1, u8 rs2)
{
	return rv_r_insn(0x20, rs2, rs1, 0, rd, 0x3b);
}

static u32 rv_sub(u8 rd, u8 rs1, u8 rs2)
{
	return rv_r_insn(0x20, rs2, rs1, 0, rd, 0x33);
}

static u32 rv_and(u8 rd, u8 rs1, u8 rs2)
{
	return rv_r_insn(0, rs2, rs1, 7, rd, 0x33);
}

static u32 rv_or(u8 rd, u8 rs1, u8 rs2)
{
	return rv_r_insn(0, rs2, rs1, 6, rd, 0x33);
}

static u32 rv_xor(u8 rd, u8 rs1, u8 rs2)
{
	return rv_r_insn(0, rs2, rs1, 4, rd, 0x33);
}

static u32 rv_mulw(u8 rd, u8 rs1, u8 rs2)
{
	return rv_r_insn(1, rs2, rs1, 0, rd, 0x3b);
}

static u32 rv_mul(u8 rd, u8 rs1, u8 rs2)
{
	return rv_r_insn(1, rs2, rs1, 0, rd, 0x33);
}

static u32 rv_divuw(u8 rd, u8 rs1, u8 rs2)
{
	return rv_r_insn(1, rs2, rs1, 5, rd, 0x3b);
}

static u32 rv_divu(u8 rd, u8 rs1, u8 rs2)
{
	return rv_r_insn(1, rs2, rs1, 5, rd, 0x33);
}

static u32 rv_remuw(u8 rd, u8 rs1, u8 rs2)
{
	return rv_r_insn(1, rs2, rs1, 7, rd, 0x3b);
}

static u32 rv_remu(u8 rd, u8 rs1, u8 rs2)
{
	return rv_r_insn(1, rs2, rs1, 7, rd, 0x33);
}

static u32 rv_sllw(u8 rd, u8 rs1, u8 rs2)
{
	return rv_r_insn(0, rs2, rs1, 1, rd, 0x3b);
}

static u32 rv_sll(u8 rd, u8 rs1, u8 rs2)
{
	return rv_r_insn(0, rs2, rs1, 1, rd, 0x33);
}

static u32 rv_srlw(u8 rd, u8 rs1, u8 rs2)
{
	return rv_r_insn(0, rs2, rs1, 5, rd, 0x3b);
}

static u32 rv_srl(u8 rd, u8 rs1, u8 rs2)
{
	return rv_r_insn(0, rs2, rs1, 5, rd, 0x33);
}

static u32 rv_sraw(u8 rd, u8 rs1, u8 rs2)
{
	return rv_r_insn(0x20, rs2, rs1, 5, rd, 0x3b);
}

static u32 rv_sra(u8 rd, u8 rs1, u8 rs2)
{
	return rv_r_insn(0x20, rs2, rs1, 5, rd, 0x33);
}

static u32 rv_lui(u8 rd, u32 imm31_12)
{
	return rv_u_insn(imm31_12, rd, 0x37);
}

static u32 rv_slli(u8 rd, u8 rs1, u16 imm11_0)
{
	return rv_i_insn(imm11_0, rs1, 1, rd, 0x13);
}

static u32 rv_andi(u8 rd, u8 rs1, u16 imm11_0)
{
	return rv_i_insn(imm11_0, rs1, 7, rd, 0x13);
}

static u32 rv_ori(u8 rd, u8 rs1, u16 imm11_0)
{
	return rv_i_insn(imm11_0, rs1, 6, rd, 0x13);
}

static u32 rv_xori(u8 rd, u8 rs1, u16 imm11_0)
{
	return rv_i_insn(imm11_0, rs1, 4, rd, 0x13);
}

static u32 rv_slliw(u8 rd, u8 rs1, u16 imm11_0)
{
	return rv_i_insn(imm11_0, rs1, 1, rd, 0x1b);
}

static u32 rv_srliw(u8 rd, u8 rs1, u16 imm11_0)
{
	return rv_i_insn(imm11_0, rs1, 5, rd, 0x1b);
}

static u32 rv_srli(u8 rd, u8 rs1, u16 imm11_0)
{
	return rv_i_insn(imm11_0, rs1, 5, rd, 0x13);
}

static u32 rv_sraiw(u8 rd, u8 rs1, u16 imm11_0)
{
	return rv_i_insn(0x400 | imm11_0, rs1, 5, rd, 0x1b);
}

static u32 rv_srai(u8 rd, u8 rs1, u16 imm11_0)
{
	return rv_i_insn(0x400 | imm11_0, rs1, 5, rd, 0x13);
}

static u32 rv_jal(u8 rd, u32 imm20_1)
{
	return rv_uj_insn(imm20_1, rd, 0x6f);
}

static u32 rv_jalr(u8 rd, u8 rs1, u16 imm11_0)
{
	return rv_i_insn(imm11_0, rs1, 0, rd, 0x67);
}

static u32 rv_beq(u8 rs1, u8 rs2, u16 imm12_1)
{
	return rv_sb_insn(imm12_1, rs2, rs1, 0, 0x63);
}

static u32 rv_bltu(u8 rs1, u8 rs2, u16 imm12_1)
{
	return rv_sb_insn(imm12_1, rs2, rs1, 6, 0x63);
}

static u32 rv_bgeu(u8 rs1, u8 rs2, u16 imm12_1)
{
	return rv_sb_insn(imm12_1, rs2, rs1, 7, 0x63);
}

static u32 rv_bne(u8 rs1, u8 rs2, u16 imm12_1)
{
	return rv_sb_insn(imm12_1, rs2, rs1, 1, 0x63);
}

static u32 rv_blt(u8 rs1, u8 rs2, u16 imm12_1)
{
	return rv_sb_insn(imm12_1, rs2, rs1, 4, 0x63);
}

static u32 rv_bge(u8 rs1, u8 rs2, u16 imm12_1)
{
	return rv_sb_insn(imm12_1, rs2, rs1, 5, 0x63);
}

static u32 rv_sb(u8 rs1, u16 imm11_0, u8 rs2)
{
	return rv_s_insn(imm11_0, rs2, rs1, 0, 0x23);
}

static u32 rv_sh(u8 rs1, u16 imm11_0, u8 rs2)
{
	return rv_s_insn(imm11_0, rs2, rs1, 1, 0x23);
}

static u32 rv_sw(u8 rs1, u16 imm11_0, u8 rs2)
{
	return rv_s_insn(imm11_0, rs2, rs1, 2, 0x23);
}

static u32 rv_sd(u8 rs1, u16 imm11_0, u8 rs2)
{
	return rv_s_insn(imm11_0, rs2, rs1, 3, 0x23);
}

static u32 rv_lbu(u8 rd, u16 imm11_0, u8 rs1)
{
	return rv_i_insn(imm11_0, rs1, 4, rd, 0x03);
}

static u32 rv_lhu(u8 rd, u16 imm11_0, u8 rs1)
{
	return rv_i_insn(imm11_0, rs1, 5, rd, 0x03);
}

static u32 rv_lwu(u8 rd, u16 imm11_0, u8 rs1)
{
	return rv_i_insn(imm11_0, rs1, 6, rd, 0x03);
}

static u32 rv_ld(u8 rd, u16 imm11_0, u8 rs1)
{
	return rv_i_insn(imm11_0, rs1, 3, rd, 0x03);
}

static u32 rv_amoadd_w(u8 rd, u8 rs2, u8 rs1, u8 aq, u8 rl)
{
	return rv_amo_insn(0, aq, rl, rs2, rs1, 2, rd, 0x2f);
}

static u32 rv_amoadd_d(u8 rd, u8 rs2, u8 rs1, u8 aq, u8 rl)
{
	return rv_amo_insn(0, aq, rl, rs2, rs1, 3, rd, 0x2f);
}

static bool is_12b_int(s64 val)
{
	return -(1 << 11) <= val && val < (1 << 11);
}

static bool is_13b_int(s64 val)
{
	return -(1 << 12) <= val && val < (1 << 12);
}

static bool is_21b_int(s64 val)
{
	return -(1L << 20) <= val && val < (1L << 20);
}

static bool is_32b_int(s64 val)
{
	return -(1L << 31) <= val && val < (1L << 31);
}

static int is_12b_check(int off, int insn)
{
	if (!is_12b_int(off)) {
		pr_err("bpf-jit: insn=%d offset=%d not supported yet!\n",
		       insn, (int)off);
		return -1;
	}
	return 0;
}

static int is_13b_check(int off, int insn)
{
	if (!is_13b_int(off)) {
		pr_err("bpf-jit: insn=%d offset=%d not supported yet!\n",
		       insn, (int)off);
		return -1;
	}
	return 0;
}

static int is_21b_check(int off, int insn)
{
	if (!is_21b_int(off)) {
		pr_err("bpf-jit: insn=%d offset=%d not supported yet!\n",
		       insn, (int)off);
		return -1;
	}
	return 0;
}

// TODO is this right?
static int __ffs(s64 num)
{
  if (num == 0)
    return -1;

  int ret = 0;
  while (num)
  {
    ret++;
    num = num >> 1;
  }

  return ret;
}

static void emit_imm(u8 rd, s64 val, struct rv_jit_context *ctx)
{
	/* Note that the immediate from the add is sign-extended,
	 * which means that we need to compensate this by adding 2^12,
	 * when the 12th bit is set. A simpler way of doing this, and
	 * getting rid of the check, is to just add 2**11 before the
	 * shift. The "Loading a 32-Bit constant" example from the
	 * "Computer Organization and Design, RISC-V edition" book by
	 * Patterson/Hennessy highlights this fact.
	 *
	 * This also means that we need to process LSB to MSB.
	 */
	s64 upper = (val + (1 << 11)) >> 12, lower = val & 0xfff;
	int shift;

	if (is_32b_int(val)) {
		if (upper)
			emit(rv_lui(rd, upper), ctx);

		if (!upper) {
			emit(rv_addi(rd, RV_REG_ZERO, lower), ctx);
			return;
		}

		emit(rv_addiw(rd, rd, lower), ctx);
		return;
	}

	shift = __ffs(upper);
	upper >>= shift;
	shift += 12;

	emit_imm(rd, upper, ctx);

	emit(rv_slli(rd, rd, shift), ctx);
	if (lower)
		emit(rv_addi(rd, rd, lower), ctx);
}

static int rv_offset(int bpf_to, int bpf_from, struct rv_jit_context *ctx)
{
	int from = ctx->offset[bpf_from] - 1, to = ctx->offset[bpf_to];

	return (to - from) << 2;
}

static int epilogue_offset(struct rv_jit_context *ctx)
{
	int to = ctx->epilogue_offset, from = ctx->ninsns;

	return (to - from) << 2;
}

static void __build_epilogue(u8 reg, struct rv_jit_context *ctx)
{
	int stack_adjust = ctx->stack_size, store_offset = stack_adjust - 8;

	if (seen_reg(RV_REG_RA, ctx)) {
		emit(rv_ld(RV_REG_RA, store_offset, RV_REG_SP), ctx);
		store_offset -= 8;
	}
	emit(rv_ld(RV_REG_FP, store_offset, RV_REG_SP), ctx);
	store_offset -= 8;
	if (seen_reg(RV_REG_S1, ctx)) {
		emit(rv_ld(RV_REG_S1, store_offset, RV_REG_SP), ctx);
		store_offset -= 8;
	}
	if (seen_reg(RV_REG_S2, ctx)) {
		emit(rv_ld(RV_REG_S2, store_offset, RV_REG_SP), ctx);
		store_offset -= 8;
	}
	if (seen_reg(RV_REG_S3, ctx)) {
		emit(rv_ld(RV_REG_S3, store_offset, RV_REG_SP), ctx);
		store_offset -= 8;
	}
	if (seen_reg(RV_REG_S4, ctx)) {
		emit(rv_ld(RV_REG_S4, store_offset, RV_REG_SP), ctx);
		store_offset -= 8;
	}
	if (seen_reg(RV_REG_S5, ctx)) {
		emit(rv_ld(RV_REG_S5, store_offset, RV_REG_SP), ctx);
		store_offset -= 8;
	}
	if (seen_reg(RV_REG_S6, ctx)) {
		emit(rv_ld(RV_REG_S6, store_offset, RV_REG_SP), ctx);
		store_offset -= 8;
	}

	emit(rv_addi(RV_REG_SP, RV_REG_SP, stack_adjust), ctx);
	/* Set return value. */
	emit(rv_addi(RV_REG_A0, RV_REG_A5, 0), ctx);
	emit(rv_jalr(RV_REG_ZERO, reg, 0), ctx);
}

static void emit_zext_32(u8 reg, struct rv_jit_context *ctx)
{
	emit(rv_slli(reg, reg, 32), ctx);
	emit(rv_srli(reg, reg, 32), ctx);
}

static int emit_bpf_tail_call(int insn, struct rv_jit_context *ctx)
{
	int tc_ninsn, off, start_insn = ctx->ninsns;
	u8 tcc = rv_tail_call_reg(ctx);

	/* a0: &ctx
	 * a1: &array
	 * a2: index
	 *
	 * if (index >= array->map.max_entries)
	 *	goto out;
	 */
	tc_ninsn = insn ? ctx->offset[insn] - ctx->offset[insn - 1] :
		   ctx->offset[0];
	emit_zext_32(RV_REG_A2, ctx);

	off = offsetof(struct bpf_array, map.max_entries);
	if (is_12b_check(off, insn))
		return -1;
	emit(rv_lwu(RV_REG_T1, off, RV_REG_A1), ctx);
	off = (tc_ninsn - (ctx->ninsns - start_insn)) << 2;
	if (is_13b_check(off, insn))
		return -1;
	emit(rv_bgeu(RV_REG_A2, RV_REG_T1, off >> 1), ctx);

	/* if (--TCC < 0)
	 *     goto out;
	 */
	emit(rv_addi(RV_REG_T1, tcc, -1), ctx);
	off = (tc_ninsn - (ctx->ninsns - start_insn)) << 2;
	if (is_13b_check(off, insn))
		return -1;
	emit(rv_blt(RV_REG_T1, RV_REG_ZERO, off >> 1), ctx);

	/* prog = array->ptrs[index];
	 * if (!prog)
	 *     goto out;
	 */
	emit(rv_slli(RV_REG_T2, RV_REG_A2, 3), ctx);
	emit(rv_add(RV_REG_T2, RV_REG_T2, RV_REG_A1), ctx);
	off = offsetof(struct bpf_array, ptrs);
	if (is_12b_check(off, insn))
		return -1;
	emit(rv_ld(RV_REG_T2, off, RV_REG_T2), ctx);
	off = (tc_ninsn - (ctx->ninsns - start_insn)) << 2;
	if (is_13b_check(off, insn))
		return -1;
	emit(rv_beq(RV_REG_T2, RV_REG_ZERO, off >> 1), ctx);

	/* goto *(prog->bpf_func + 4); */
	off = offsetof(struct bpf_prog, bpf_func);
	if (is_12b_check(off, insn))
		return -1;
	emit(rv_ld(RV_REG_T3, off, RV_REG_T2), ctx);
	emit(rv_addi(RV_REG_T3, RV_REG_T3, 4), ctx);
	emit(rv_addi(RV_REG_TCC, RV_REG_T1, 0), ctx);
	__build_epilogue(RV_REG_T3, ctx);
	return 0;
}

static void init_regs(u8 *rd, u8 *rs, const struct bpf_insn *insn,
		      struct rv_jit_context *ctx)
{
	u8 code = insn->code;

	switch (code) {
	case BPF_JMP | BPF_JA:
	case BPF_JMP | BPF_CALL:
	case BPF_JMP | BPF_EXIT:
	case BPF_JMP | BPF_TAIL_CALL:
		break;
	default:
		*rd = bpf_to_rv_reg(insn->dst_reg, ctx);
	}

	if (code & (BPF_ALU | BPF_X) || code & (BPF_ALU64 | BPF_X) ||
	    code & (BPF_JMP | BPF_X) || code & (BPF_JMP32 | BPF_X) ||
	    code & BPF_LDX || code & BPF_STX)
		*rs = bpf_to_rv_reg(insn->src_reg, ctx);
}

static int rv_offset_check(int *rvoff, s16 off, int insn,
			   struct rv_jit_context *ctx)
{
	*rvoff = rv_offset(insn + off, insn, ctx);
	return is_13b_check(*rvoff, insn);
}

static void emit_zext_32_rd_rs(u8 *rd, u8 *rs, struct rv_jit_context *ctx)
{
	emit(rv_addi(RV_REG_T2, *rd, 0), ctx);
	emit_zext_32(RV_REG_T2, ctx);
	emit(rv_addi(RV_REG_T1, *rs, 0), ctx);
	emit_zext_32(RV_REG_T1, ctx);
	*rd = RV_REG_T2;
	*rs = RV_REG_T1;
}

static void emit_sext_32_rd_rs(u8 *rd, u8 *rs, struct rv_jit_context *ctx)
{
	emit(rv_addiw(RV_REG_T2, *rd, 0), ctx);
	emit(rv_addiw(RV_REG_T1, *rs, 0), ctx);
	*rd = RV_REG_T2;
	*rs = RV_REG_T1;
}

static void emit_zext_32_rd_t1(u8 *rd, struct rv_jit_context *ctx)
{
	emit(rv_addi(RV_REG_T2, *rd, 0), ctx);
	emit_zext_32(RV_REG_T2, ctx);
	emit_zext_32(RV_REG_T1, ctx);
	*rd = RV_REG_T2;
}

static void emit_sext_32_rd(u8 *rd, struct rv_jit_context *ctx)
{
	emit(rv_addiw(RV_REG_T2, *rd, 0), ctx);
	*rd = RV_REG_T2;
}

// ret = bpf_jit_get_func_addr(ctx->prog, insn, extra_pass, &addr,
// &fixed);
static int bpf_jit_get_func_addr(struct bpf_prog *prog, struct bpf_insn *insn, bool extra_pass,
    u64 *addr, bool *fixed)
{
  // TODO (don't implement for now, but just don't use function calls)
  // Will need to implement when comparing function calls
  return 0;
}

static int emit_insn(const struct bpf_insn *insn, struct rv_jit_context *ctx,
		     bool extra_pass)
{
	bool is64 = BPF_CLASS(insn->code) == BPF_ALU64 ||
		    BPF_CLASS(insn->code) == BPF_JMP;
	int rvoff, i = insn - ctx->prog->insnsi;
	u8 rd = -1, rs = -1, code = insn->code;
	s16 off = insn->off;
	s32 imm = insn->imm;

	init_regs(&rd, &rs, insn, ctx);

  pr_err("BPF opcode: 0x%02X\n", code);
	switch (code) {
	/* dst = src */
	case BPF_ALU | BPF_MOV | BPF_X:
	case BPF_ALU64 | BPF_MOV | BPF_X:
		emit(is64 ? rv_addi(rd, rs, 0) : rv_addiw(rd, rs, 0), ctx);
		if (!is64)
			emit_zext_32(rd, ctx);
		break;

	/* dst = dst OP src */
	case BPF_ALU | BPF_ADD | BPF_X:
	case BPF_ALU64 | BPF_ADD | BPF_X:
		emit(is64 ? rv_add(rd, rd, rs) : rv_addw(rd, rd, rs), ctx);
		break;
	case BPF_ALU | BPF_SUB | BPF_X:
	case BPF_ALU64 | BPF_SUB | BPF_X:
		emit(is64 ? rv_sub(rd, rd, rs) : rv_subw(rd, rd, rs), ctx);
		break;
	case BPF_ALU | BPF_AND | BPF_X:
	case BPF_ALU64 | BPF_AND | BPF_X:
		emit(rv_and(rd, rd, rs), ctx);
		break;
	case BPF_ALU | BPF_OR | BPF_X:
	case BPF_ALU64 | BPF_OR | BPF_X:
		emit(rv_or(rd, rd, rs), ctx);
		break;
	case BPF_ALU | BPF_XOR | BPF_X:
	case BPF_ALU64 | BPF_XOR | BPF_X:
		emit(rv_xor(rd, rd, rs), ctx);
		break;
	case BPF_ALU | BPF_MUL | BPF_X:
	case BPF_ALU64 | BPF_MUL | BPF_X:
		emit(is64 ? rv_mul(rd, rd, rs) : rv_mulw(rd, rd, rs), ctx);
		if (!is64)
			emit_zext_32(rd, ctx);
		break;
	case BPF_ALU | BPF_DIV | BPF_X:
	case BPF_ALU64 | BPF_DIV | BPF_X:
		emit(is64 ? rv_divu(rd, rd, rs) : rv_divuw(rd, rd, rs), ctx);
		if (!is64)
			emit_zext_32(rd, ctx);
		break;
	case BPF_ALU | BPF_MOD | BPF_X:
	case BPF_ALU64 | BPF_MOD | BPF_X:
		emit(is64 ? rv_remu(rd, rd, rs) : rv_remuw(rd, rd, rs), ctx);
		if (!is64)
			emit_zext_32(rd, ctx);
		break;
	case BPF_ALU | BPF_LSH | BPF_X:
	case BPF_ALU64 | BPF_LSH | BPF_X:
		emit(is64 ? rv_sll(rd, rd, rs) : rv_sllw(rd, rd, rs), ctx);
		break;
	case BPF_ALU | BPF_RSH | BPF_X:
	case BPF_ALU64 | BPF_RSH | BPF_X:
		emit(is64 ? rv_srl(rd, rd, rs) : rv_srlw(rd, rd, rs), ctx);
		break;
	case BPF_ALU | BPF_ARSH | BPF_X:
	case BPF_ALU64 | BPF_ARSH | BPF_X:
		emit(is64 ? rv_sra(rd, rd, rs) : rv_sraw(rd, rd, rs), ctx);
		break;

	/* dst = -dst */
	case BPF_ALU | BPF_NEG:
	case BPF_ALU64 | BPF_NEG:
		emit(is64 ? rv_sub(rd, RV_REG_ZERO, rd) :
		     rv_subw(rd, RV_REG_ZERO, rd), ctx);
		break;

	/* dst = BSWAP##imm(dst) */
	case BPF_ALU | BPF_END | BPF_FROM_LE:
	{
		int shift = 64 - imm;

		emit(rv_slli(rd, rd, shift), ctx);
		emit(rv_srli(rd, rd, shift), ctx);
		break;
	}
	case BPF_ALU | BPF_END | BPF_FROM_BE:
		emit(rv_addi(RV_REG_T2, RV_REG_ZERO, 0), ctx);

		emit(rv_andi(RV_REG_T1, rd, 0xff), ctx);
		emit(rv_add(RV_REG_T2, RV_REG_T2, RV_REG_T1), ctx);
		emit(rv_slli(RV_REG_T2, RV_REG_T2, 8), ctx);
		emit(rv_srli(rd, rd, 8), ctx);
		if (imm == 16)
			goto out_be;

		emit(rv_andi(RV_REG_T1, rd, 0xff), ctx);
		emit(rv_add(RV_REG_T2, RV_REG_T2, RV_REG_T1), ctx);
		emit(rv_slli(RV_REG_T2, RV_REG_T2, 8), ctx);
		emit(rv_srli(rd, rd, 8), ctx);

		emit(rv_andi(RV_REG_T1, rd, 0xff), ctx);
		emit(rv_add(RV_REG_T2, RV_REG_T2, RV_REG_T1), ctx);
		emit(rv_slli(RV_REG_T2, RV_REG_T2, 8), ctx);
		emit(rv_srli(rd, rd, 8), ctx);
		if (imm == 32)
			goto out_be;

		emit(rv_andi(RV_REG_T1, rd, 0xff), ctx);
		emit(rv_add(RV_REG_T2, RV_REG_T2, RV_REG_T1), ctx);
		emit(rv_slli(RV_REG_T2, RV_REG_T2, 8), ctx);
		emit(rv_srli(rd, rd, 8), ctx);

		emit(rv_andi(RV_REG_T1, rd, 0xff), ctx);
		emit(rv_add(RV_REG_T2, RV_REG_T2, RV_REG_T1), ctx);
		emit(rv_slli(RV_REG_T2, RV_REG_T2, 8), ctx);
		emit(rv_srli(rd, rd, 8), ctx);

		emit(rv_andi(RV_REG_T1, rd, 0xff), ctx);
		emit(rv_add(RV_REG_T2, RV_REG_T2, RV_REG_T1), ctx);
		emit(rv_slli(RV_REG_T2, RV_REG_T2, 8), ctx);
		emit(rv_srli(rd, rd, 8), ctx);

		emit(rv_andi(RV_REG_T1, rd, 0xff), ctx);
		emit(rv_add(RV_REG_T2, RV_REG_T2, RV_REG_T1), ctx);
		emit(rv_slli(RV_REG_T2, RV_REG_T2, 8), ctx);
		emit(rv_srli(rd, rd, 8), ctx);
out_be:
		emit(rv_andi(RV_REG_T1, rd, 0xff), ctx);
		emit(rv_add(RV_REG_T2, RV_REG_T2, RV_REG_T1), ctx);

		emit(rv_addi(rd, RV_REG_T2, 0), ctx);
		break;

	/* dst = imm */
	case BPF_ALU | BPF_MOV | BPF_K:
	case BPF_ALU64 | BPF_MOV | BPF_K:
		emit_imm(rd, imm, ctx);
		if (!is64)
			emit_zext_32(rd, ctx);
		break;

	/* dst = dst OP imm */
	case BPF_ALU | BPF_ADD | BPF_K:
	case BPF_ALU64 | BPF_ADD | BPF_K:
		if (is_12b_int(imm)) {
			emit(is64 ? rv_addi(rd, rd, imm) :
			     rv_addiw(rd, rd, imm), ctx);
		} else {
			emit_imm(RV_REG_T1, imm, ctx);
			emit(is64 ? rv_add(rd, rd, RV_REG_T1) :
			     rv_addw(rd, rd, RV_REG_T1), ctx);
		}
		if (!is64)
			emit_zext_32(rd, ctx);
		break;
	case BPF_ALU | BPF_SUB | BPF_K:
	case BPF_ALU64 | BPF_SUB | BPF_K:
		if (is_12b_int(-imm)) {
			emit(is64 ? rv_addi(rd, rd, -imm) :
			     rv_addiw(rd, rd, -imm), ctx);
		} else {
			emit_imm(RV_REG_T1, imm, ctx);
			emit(is64 ? rv_sub(rd, rd, RV_REG_T1) :
			     rv_subw(rd, rd, RV_REG_T1), ctx);
		}
		if (!is64)
			emit_zext_32(rd, ctx);
		break;
	case BPF_ALU | BPF_AND | BPF_K:
	case BPF_ALU64 | BPF_AND | BPF_K:
		if (is_12b_int(imm)) {
			emit(rv_andi(rd, rd, imm), ctx);
		} else {
			emit_imm(RV_REG_T1, imm, ctx);
			emit(rv_and(rd, rd, RV_REG_T1), ctx);
		}
		if (!is64)
			emit_zext_32(rd, ctx);
		break;
	case BPF_ALU | BPF_OR | BPF_K:
	case BPF_ALU64 | BPF_OR | BPF_K:
		if (is_12b_int(imm)) {
			emit(rv_ori(rd, rd, imm), ctx);
		} else {
			emit_imm(RV_REG_T1, imm, ctx);
			emit(rv_or(rd, rd, RV_REG_T1), ctx);
		}
		if (!is64)
			emit_zext_32(rd, ctx);
		break;
	case BPF_ALU | BPF_XOR | BPF_K:
	case BPF_ALU64 | BPF_XOR | BPF_K:
		if (is_12b_int(imm)) {
			emit(rv_xori(rd, rd, imm), ctx);
		} else {
			emit_imm(RV_REG_T1, imm, ctx);
			emit(rv_xor(rd, rd, RV_REG_T1), ctx);
		}
		if (!is64)
			emit_zext_32(rd, ctx);
		break;
	case BPF_ALU | BPF_MUL | BPF_K:
	case BPF_ALU64 | BPF_MUL | BPF_K:
		emit_imm(RV_REG_T1, imm, ctx);
		emit(is64 ? rv_mul(rd, rd, RV_REG_T1) :
		     rv_mulw(rd, rd, RV_REG_T1), ctx);
		if (!is64)
			emit_zext_32(rd, ctx);
		break;
	case BPF_ALU | BPF_DIV | BPF_K:
	case BPF_ALU64 | BPF_DIV | BPF_K:
		emit_imm(RV_REG_T1, imm, ctx);
		emit(is64 ? rv_divu(rd, rd, RV_REG_T1) :
		     rv_divuw(rd, rd, RV_REG_T1), ctx);
		if (!is64)
			emit_zext_32(rd, ctx);
		break;
	case BPF_ALU | BPF_MOD | BPF_K:
	case BPF_ALU64 | BPF_MOD | BPF_K:
		emit_imm(RV_REG_T1, imm, ctx);
		emit(is64 ? rv_remu(rd, rd, RV_REG_T1) :
		     rv_remuw(rd, rd, RV_REG_T1), ctx);
		if (!is64)
			emit_zext_32(rd, ctx);
		break;
	case BPF_ALU | BPF_LSH | BPF_K:
	case BPF_ALU64 | BPF_LSH | BPF_K:
		emit(is64 ? rv_slli(rd, rd, imm) : rv_slliw(rd, rd, imm), ctx);
		break;
	case BPF_ALU | BPF_RSH | BPF_K:
	case BPF_ALU64 | BPF_RSH | BPF_K:
		emit(is64 ? rv_srli(rd, rd, imm) : rv_srliw(rd, rd, imm), ctx);
		break;
	case BPF_ALU | BPF_ARSH | BPF_K:
	case BPF_ALU64 | BPF_ARSH | BPF_K:
		emit(is64 ? rv_srai(rd, rd, imm) : rv_sraiw(rd, rd, imm), ctx);
		break;

	/* JUMP off */
	case BPF_JMP | BPF_JA:
		rvoff = rv_offset(i + off, i, ctx);
		if (!is_21b_int(rvoff)) {
			pr_err("bpf-jit: insn=%d offset=%d not supported yet!\n",
			       i, rvoff);
			return -1;
		}

		emit(rv_jal(RV_REG_ZERO, rvoff >> 1), ctx);
		break;

	/* IF (dst COND src) JUMP off */
	case BPF_JMP | BPF_JEQ | BPF_X:
	case BPF_JMP32 | BPF_JEQ | BPF_X:
		if (rv_offset_check(&rvoff, off, i, ctx))
			return -1;
		if (!is64)
			emit_zext_32_rd_rs(&rd, &rs, ctx);
		emit(rv_beq(rd, rs, rvoff >> 1), ctx);
		break;
	case BPF_JMP | BPF_JGT | BPF_X:
	case BPF_JMP32 | BPF_JGT | BPF_X:
		if (rv_offset_check(&rvoff, off, i, ctx))
			return -1;
		if (!is64)
			emit_zext_32_rd_rs(&rd, &rs, ctx);
		emit(rv_bltu(rs, rd, rvoff >> 1), ctx);
		break;
	case BPF_JMP | BPF_JLT | BPF_X:
	case BPF_JMP32 | BPF_JLT | BPF_X:
		if (rv_offset_check(&rvoff, off, i, ctx))
			return -1;
		if (!is64)
			emit_zext_32_rd_rs(&rd, &rs, ctx);
		emit(rv_bltu(rd, rs, rvoff >> 1), ctx);
		break;
	case BPF_JMP | BPF_JGE | BPF_X:
	case BPF_JMP32 | BPF_JGE | BPF_X:
		if (rv_offset_check(&rvoff, off, i, ctx))
			return -1;
		if (!is64)
			emit_zext_32_rd_rs(&rd, &rs, ctx);
		emit(rv_bgeu(rd, rs, rvoff >> 1), ctx);
		break;
	case BPF_JMP | BPF_JLE | BPF_X:
	case BPF_JMP32 | BPF_JLE | BPF_X:
		if (rv_offset_check(&rvoff, off, i, ctx))
			return -1;
		if (!is64)
			emit_zext_32_rd_rs(&rd, &rs, ctx);
		emit(rv_bgeu(rs, rd, rvoff >> 1), ctx);
		break;
	case BPF_JMP | BPF_JNE | BPF_X:
	case BPF_JMP32 | BPF_JNE | BPF_X:
		if (rv_offset_check(&rvoff, off, i, ctx))
			return -1;
		if (!is64)
			emit_zext_32_rd_rs(&rd, &rs, ctx);
		emit(rv_bne(rd, rs, rvoff >> 1), ctx);
		break;
	case BPF_JMP | BPF_JSGT | BPF_X:
	case BPF_JMP32 | BPF_JSGT | BPF_X:
		if (rv_offset_check(&rvoff, off, i, ctx))
			return -1;
		if (!is64)
			emit_sext_32_rd_rs(&rd, &rs, ctx);
		emit(rv_blt(rs, rd, rvoff >> 1), ctx);
		break;
	case BPF_JMP | BPF_JSLT | BPF_X:
	case BPF_JMP32 | BPF_JSLT | BPF_X:
		if (rv_offset_check(&rvoff, off, i, ctx))
			return -1;
		if (!is64)
			emit_sext_32_rd_rs(&rd, &rs, ctx);
		emit(rv_blt(rd, rs, rvoff >> 1), ctx);
		break;
	case BPF_JMP | BPF_JSGE | BPF_X:
	case BPF_JMP32 | BPF_JSGE | BPF_X:
		if (rv_offset_check(&rvoff, off, i, ctx))
			return -1;
		if (!is64)
			emit_sext_32_rd_rs(&rd, &rs, ctx);
		emit(rv_bge(rd, rs, rvoff >> 1), ctx);
		break;
	case BPF_JMP | BPF_JSLE | BPF_X:
	case BPF_JMP32 | BPF_JSLE | BPF_X:
		if (rv_offset_check(&rvoff, off, i, ctx))
			return -1;
		if (!is64)
			emit_sext_32_rd_rs(&rd, &rs, ctx);
		emit(rv_bge(rs, rd, rvoff >> 1), ctx);
		break;
	case BPF_JMP | BPF_JSET | BPF_X:
	case BPF_JMP32 | BPF_JSET | BPF_X:
		if (rv_offset_check(&rvoff, off, i, ctx))
			return -1;
		if (!is64)
			emit_zext_32_rd_rs(&rd, &rs, ctx);
		emit(rv_and(RV_REG_T1, rd, rs), ctx);
		emit(rv_bne(RV_REG_T1, RV_REG_ZERO, rvoff >> 1), ctx);
		break;

	/* IF (dst COND imm) JUMP off */
	case BPF_JMP | BPF_JEQ | BPF_K:
	case BPF_JMP32 | BPF_JEQ | BPF_K:
		if (rv_offset_check(&rvoff, off, i, ctx))
			return -1;
		emit_imm(RV_REG_T1, imm, ctx);
		if (!is64)
			emit_zext_32_rd_t1(&rd, ctx);
		emit(rv_beq(rd, RV_REG_T1, rvoff >> 1), ctx);
		break;
	case BPF_JMP | BPF_JGT | BPF_K:
	case BPF_JMP32 | BPF_JGT | BPF_K:
		if (rv_offset_check(&rvoff, off, i, ctx))
			return -1;
		emit_imm(RV_REG_T1, imm, ctx);
		if (!is64)
			emit_zext_32_rd_t1(&rd, ctx);
		emit(rv_bltu(RV_REG_T1, rd, rvoff >> 1), ctx);
		break;
	case BPF_JMP | BPF_JLT | BPF_K:
	case BPF_JMP32 | BPF_JLT | BPF_K:
		if (rv_offset_check(&rvoff, off, i, ctx))
			return -1;
		emit_imm(RV_REG_T1, imm, ctx);
		if (!is64)
			emit_zext_32_rd_t1(&rd, ctx);
		emit(rv_bltu(rd, RV_REG_T1, rvoff >> 1), ctx);
		break;
	case BPF_JMP | BPF_JGE | BPF_K:
	case BPF_JMP32 | BPF_JGE | BPF_K:
		if (rv_offset_check(&rvoff, off, i, ctx))
			return -1;
		emit_imm(RV_REG_T1, imm, ctx);
		if (!is64)
			emit_zext_32_rd_t1(&rd, ctx);
		emit(rv_bgeu(rd, RV_REG_T1, rvoff >> 1), ctx);
		break;
	case BPF_JMP | BPF_JLE | BPF_K:
	case BPF_JMP32 | BPF_JLE | BPF_K:
		if (rv_offset_check(&rvoff, off, i, ctx))
			return -1;
		emit_imm(RV_REG_T1, imm, ctx);
		if (!is64)
			emit_zext_32_rd_t1(&rd, ctx);
		emit(rv_bgeu(RV_REG_T1, rd, rvoff >> 1), ctx);
		break;
	case BPF_JMP | BPF_JNE | BPF_K:
	case BPF_JMP32 | BPF_JNE | BPF_K:
		if (rv_offset_check(&rvoff, off, i, ctx))
			return -1;
		emit_imm(RV_REG_T1, imm, ctx);
		if (!is64)
			emit_zext_32_rd_t1(&rd, ctx);
		emit(rv_bne(rd, RV_REG_T1, rvoff >> 1), ctx);
		break;
	case BPF_JMP | BPF_JSGT | BPF_K:
	case BPF_JMP32 | BPF_JSGT | BPF_K:
		if (rv_offset_check(&rvoff, off, i, ctx))
			return -1;
		emit_imm(RV_REG_T1, imm, ctx);
		if (!is64)
			emit_sext_32_rd(&rd, ctx);
		emit(rv_blt(RV_REG_T1, rd, rvoff >> 1), ctx);
		break;
	case BPF_JMP | BPF_JSLT | BPF_K:
	case BPF_JMP32 | BPF_JSLT | BPF_K:
		if (rv_offset_check(&rvoff, off, i, ctx))
			return -1;
		emit_imm(RV_REG_T1, imm, ctx);
		if (!is64)
			emit_sext_32_rd(&rd, ctx);
		emit(rv_blt(rd, RV_REG_T1, rvoff >> 1), ctx);
		break;
	case BPF_JMP | BPF_JSGE | BPF_K:
	case BPF_JMP32 | BPF_JSGE | BPF_K:
		if (rv_offset_check(&rvoff, off, i, ctx))
			return -1;
		emit_imm(RV_REG_T1, imm, ctx);
		if (!is64)
			emit_sext_32_rd(&rd, ctx);
		emit(rv_bge(rd, RV_REG_T1, rvoff >> 1), ctx);
		break;
	case BPF_JMP | BPF_JSLE | BPF_K:
	case BPF_JMP32 | BPF_JSLE | BPF_K:
		if (rv_offset_check(&rvoff, off, i, ctx))
			return -1;
		emit_imm(RV_REG_T1, imm, ctx);
		if (!is64)
			emit_sext_32_rd(&rd, ctx);
		emit(rv_bge(RV_REG_T1, rd, rvoff >> 1), ctx);
		break;
	case BPF_JMP | BPF_JSET | BPF_K:
	case BPF_JMP32 | BPF_JSET | BPF_K:
		if (rv_offset_check(&rvoff, off, i, ctx))
			return -1;
		emit_imm(RV_REG_T1, imm, ctx);
		if (!is64)
			emit_zext_32_rd_t1(&rd, ctx);
		emit(rv_and(RV_REG_T1, rd, RV_REG_T1), ctx);
		emit(rv_bne(RV_REG_T1, RV_REG_ZERO, rvoff >> 1), ctx);
		break;

	/* function call */
	case BPF_JMP | BPF_CALL:
	{
		bool fixed;
		int i, ret;
		u64 addr;

		mark_call(ctx);
		ret = bpf_jit_get_func_addr(ctx->prog, insn, extra_pass, &addr,
					    &fixed);
    // If error
		if (ret < 0)
			return ret;
		if (fixed) {
			emit_imm(RV_REG_T1, addr, ctx);
		} else {
			i = ctx->ninsns;
			emit_imm(RV_REG_T1, addr, ctx);
			for (i = ctx->ninsns - i; i < 8; i++) {
				/* nop */
				emit(rv_addi(RV_REG_ZERO, RV_REG_ZERO, 0),
				     ctx);
			}
		}
		emit(rv_jalr(RV_REG_RA, RV_REG_T1, 0), ctx);
		rd = bpf_to_rv_reg(0, ctx);
		emit(rv_addi(rd, RV_REG_A0, 0), ctx);
		break;
	}
	/* tail call */
	case BPF_JMP | BPF_TAIL_CALL:
		if (emit_bpf_tail_call(i, ctx))
			return -1;
		break;

	/* function return */
	case BPF_JMP | BPF_EXIT:
		if (i == ctx->prog->len - 1)
			break;

		rvoff = epilogue_offset(ctx);
		if (is_21b_check(rvoff, i))
			return -1;
		emit(rv_jal(RV_REG_ZERO, rvoff >> 1), ctx);
		break;

	/* dst = imm64 */
	case BPF_LD | BPF_IMM | BPF_DW:
	{
		struct bpf_insn insn1 = insn[1];
		u64 imm64;

		imm64 = (u64)insn1.imm << 32 | (u32)imm;
		emit_imm(rd, imm64, ctx);
		return 1;
	}

	/* LDX: dst = *(size *)(src + off) */
	case BPF_LDX | BPF_MEM | BPF_B:
		if (is_12b_int(off)) {
			emit(rv_lbu(rd, off, rs), ctx);
			break;
		}

		emit_imm(RV_REG_T1, off, ctx);
		emit(rv_add(RV_REG_T1, RV_REG_T1, rs), ctx);
		emit(rv_lbu(rd, 0, RV_REG_T1), ctx);
		break;
	case BPF_LDX | BPF_MEM | BPF_H:
		if (is_12b_int(off)) {
			emit(rv_lhu(rd, off, rs), ctx);
			break;
		}

		emit_imm(RV_REG_T1, off, ctx);
		emit(rv_add(RV_REG_T1, RV_REG_T1, rs), ctx);
		emit(rv_lhu(rd, 0, RV_REG_T1), ctx);
		break;
	case BPF_LDX | BPF_MEM | BPF_W:
		if (is_12b_int(off)) {
			emit(rv_lwu(rd, off, rs), ctx);
			break;
		}

		emit_imm(RV_REG_T1, off, ctx);
		emit(rv_add(RV_REG_T1, RV_REG_T1, rs), ctx);
		emit(rv_lwu(rd, 0, RV_REG_T1), ctx);
		break;
	case BPF_LDX | BPF_MEM | BPF_DW:
		if (is_12b_int(off)) {
			emit(rv_ld(rd, off, rs), ctx);
			break;
		}

		emit_imm(RV_REG_T1, off, ctx);
		emit(rv_add(RV_REG_T1, RV_REG_T1, rs), ctx);
		emit(rv_ld(rd, 0, RV_REG_T1), ctx);
		break;

	/* ST: *(size *)(dst + off) = imm */
	case BPF_ST | BPF_MEM | BPF_B:
		emit_imm(RV_REG_T1, imm, ctx);
		if (is_12b_int(off)) {
			emit(rv_sb(rd, off, RV_REG_T1), ctx);
			break;
		}

		emit_imm(RV_REG_T2, off, ctx);
		emit(rv_add(RV_REG_T2, RV_REG_T2, rd), ctx);
		emit(rv_sb(RV_REG_T2, 0, RV_REG_T1), ctx);
		break;

	case BPF_ST | BPF_MEM | BPF_H:
		emit_imm(RV_REG_T1, imm, ctx);
		if (is_12b_int(off)) {
			emit(rv_sh(rd, off, RV_REG_T1), ctx);
			break;
		}

		emit_imm(RV_REG_T2, off, ctx);
		emit(rv_add(RV_REG_T2, RV_REG_T2, rd), ctx);
		emit(rv_sh(RV_REG_T2, 0, RV_REG_T1), ctx);
		break;
	case BPF_ST | BPF_MEM | BPF_W:
		emit_imm(RV_REG_T1, imm, ctx);
		if (is_12b_int(off)) {
			emit(rv_sw(rd, off, RV_REG_T1), ctx);
			break;
		}

		emit_imm(RV_REG_T2, off, ctx);
		emit(rv_add(RV_REG_T2, RV_REG_T2, rd), ctx);
		emit(rv_sw(RV_REG_T2, 0, RV_REG_T1), ctx);
		break;
	case BPF_ST | BPF_MEM | BPF_DW:
		emit_imm(RV_REG_T1, imm, ctx);
		if (is_12b_int(off)) {
			emit(rv_sd(rd, off, RV_REG_T1), ctx);
			break;
		}

		emit_imm(RV_REG_T2, off, ctx);
		emit(rv_add(RV_REG_T2, RV_REG_T2, rd), ctx);
		emit(rv_sd(RV_REG_T2, 0, RV_REG_T1), ctx);
		break;

	/* STX: *(size *)(dst + off) = src */
	case BPF_STX | BPF_MEM | BPF_B:
		if (is_12b_int(off)) {
			emit(rv_sb(rd, off, rs), ctx);
			break;
		}

		emit_imm(RV_REG_T1, off, ctx);
		emit(rv_add(RV_REG_T1, RV_REG_T1, rd), ctx);
		emit(rv_sb(RV_REG_T1, 0, rs), ctx);
		break;
	case BPF_STX | BPF_MEM | BPF_H:
		if (is_12b_int(off)) {
			emit(rv_sh(rd, off, rs), ctx);
			break;
		}

		emit_imm(RV_REG_T1, off, ctx);
		emit(rv_add(RV_REG_T1, RV_REG_T1, rd), ctx);
		emit(rv_sh(RV_REG_T1, 0, rs), ctx);
		break;
	case BPF_STX | BPF_MEM | BPF_W:
		if (is_12b_int(off)) {
			emit(rv_sw(rd, off, rs), ctx);
			break;
		}

		emit_imm(RV_REG_T1, off, ctx);
		emit(rv_add(RV_REG_T1, RV_REG_T1, rd), ctx);
		emit(rv_sw(RV_REG_T1, 0, rs), ctx);
		break;
	case BPF_STX | BPF_MEM | BPF_DW:
		if (is_12b_int(off)) {
			emit(rv_sd(rd, off, rs), ctx);
			break;
		}

		emit_imm(RV_REG_T1, off, ctx);
		emit(rv_add(RV_REG_T1, RV_REG_T1, rd), ctx);
		emit(rv_sd(RV_REG_T1, 0, rs), ctx);
		break;
	/* STX XADD: lock *(u32 *)(dst + off) += src */
	case BPF_STX | BPF_XADD | BPF_W:
	/* STX XADD: lock *(u64 *)(dst + off) += src */
	case BPF_STX | BPF_XADD | BPF_DW:
		if (off) {
			if (is_12b_int(off)) {
				emit(rv_addi(RV_REG_T1, rd, off), ctx);
			} else {
				emit_imm(RV_REG_T1, off, ctx);
				emit(rv_add(RV_REG_T1, RV_REG_T1, rd), ctx);
			}

			rd = RV_REG_T1;
		}

		emit(BPF_SIZE(code) == BPF_W ?
		     rv_amoadd_w(RV_REG_ZERO, rs, rd, 0, 0) :
		     rv_amoadd_d(RV_REG_ZERO, rs, rd, 0, 0), ctx);
		break;
	default:
		pr_err("bpf-jit: unknown opcode %02x\n", code);
    return -1; // TODO this error code ok?
		// return -EINVAL;
	}

	return 0;
}

static int round_up(int N, int S)
{
  pr_err("Round up, N: %d, S: %d\n", N, S);
  return (((N + S - 1) / S) * S);
}

static void build_prologue(struct rv_jit_context *ctx)
{
  pr_comm("Doing build_prologue\n");
	int stack_adjust = 0, store_offset, bpf_stack_adjust;

	if (seen_reg(RV_REG_RA, ctx))
		stack_adjust += 8;
	stack_adjust += 8; // RV_REG_FP
	if (seen_reg(RV_REG_S1, ctx))
		stack_adjust += 8;
	if (seen_reg(RV_REG_S2, ctx))
		stack_adjust += 8;
	if (seen_reg(RV_REG_S3, ctx))
		stack_adjust += 8;
	if (seen_reg(RV_REG_S4, ctx))
		stack_adjust += 8;
	if (seen_reg(RV_REG_S5, ctx))
		stack_adjust += 8;
	if (seen_reg(RV_REG_S6, ctx))
		stack_adjust += 8;
  
  pr_comm("here\n");

	stack_adjust = round_up(stack_adjust, 16);
	bpf_stack_adjust = round_up(ctx->prog->aux->stack_depth, 16);
	stack_adjust += bpf_stack_adjust;
  pr_comm("here");

	store_offset = stack_adjust - 8;

	/* First instruction is always setting the tail-call-counter
	 * (TCC) register. This instruction is skipped for tail calls.
	 */
	emit(rv_addi(RV_REG_TCC, RV_REG_ZERO, MAX_TAIL_CALL_CNT), ctx);

	emit(rv_addi(RV_REG_SP, RV_REG_SP, -stack_adjust), ctx);

	if (seen_reg(RV_REG_RA, ctx)) {
		emit(rv_sd(RV_REG_SP, store_offset, RV_REG_RA), ctx);
		store_offset -= 8;
	}
	emit(rv_sd(RV_REG_SP, store_offset, RV_REG_FP), ctx);
	store_offset -= 8;
	if (seen_reg(RV_REG_S1, ctx)) {
		emit(rv_sd(RV_REG_SP, store_offset, RV_REG_S1), ctx);
		store_offset -= 8;
	}
	if (seen_reg(RV_REG_S2, ctx)) {
		emit(rv_sd(RV_REG_SP, store_offset, RV_REG_S2), ctx);
		store_offset -= 8;
	}
	if (seen_reg(RV_REG_S3, ctx)) {
		emit(rv_sd(RV_REG_SP, store_offset, RV_REG_S3), ctx);
		store_offset -= 8;
	}
	if (seen_reg(RV_REG_S4, ctx)) {
		emit(rv_sd(RV_REG_SP, store_offset, RV_REG_S4), ctx);
		store_offset -= 8;
	}
	if (seen_reg(RV_REG_S5, ctx)) {
		emit(rv_sd(RV_REG_SP, store_offset, RV_REG_S5), ctx);
		store_offset -= 8;
	}
	if (seen_reg(RV_REG_S6, ctx)) {
		emit(rv_sd(RV_REG_SP, store_offset, RV_REG_S6), ctx);
		store_offset -= 8;
	}

  pr_comm("here");

	emit(rv_addi(RV_REG_FP, RV_REG_SP, stack_adjust), ctx);

	if (bpf_stack_adjust)
		emit(rv_addi(RV_REG_S5, RV_REG_SP, bpf_stack_adjust), ctx);

	/* Program contains calls and tail calls, so RV_REG_TCC need
	 * to be saved across calls.
	 */
	if (seen_tail_call(ctx) && seen_call(ctx))
		emit(rv_addi(RV_REG_TCC_SAVED, RV_REG_TCC, 0), ctx);

	ctx->stack_size = stack_adjust;
  pr_comm("here");
}

static void build_epilogue(struct rv_jit_context *ctx)
{
  pr_comm("Doing build_epilogue\n");
	__build_epilogue(RV_REG_RA, ctx);
}

static int build_body(struct rv_jit_context *ctx, bool extra_pass)
{
  pr_comm("Doing build_body\n");
	const struct bpf_prog *prog = ctx->prog;
	int i;

	for (i = 0; i < prog->len; i++) {
		const struct bpf_insn *insn = &prog->insnsi[i];
		int ret;

		ret = emit_insn(insn, ctx, extra_pass);
		if (ret > 0) {
			i++;
			if (ctx->insns == NULL)
				ctx->offset[i] = ctx->ninsns;
			continue;
		}
		if (ctx->insns == NULL)
			ctx->offset[i] = ctx->ninsns;
		if (ret)
			return ret;
	}
	return 0;
}

static void bpf_fill_ill_insns(void *area, unsigned int size)
{
	memset(area, 0, size);
}

static void bpf_flush_icache(void *start, void *end)
{
  // TODO
	// flush_icache_range((unsigned long)start, (unsigned long)end);
  // NOTE: This does nothing in the source, so should be fine?
}

static bool IS_ERR(struct bpf_prog *prog)
{
  // TODO need to impl?
  return prog == NULL;
}

// Actual implementation fills some space with illegal instructions? Why?
static struct bpf_binary_header* bpf_jit_binary_alloc(unsigned int proglen, u8 **image_ptr, int alignment)
{
  // TODO call bpf_fill_ill_insns somehow?
	struct bpf_binary_header *hdr;
  int PAGE_SIZE = 128; // TODO how correct is this? idk
	u32 size = round_up(proglen + sizeof(*hdr) + 128, PAGE_SIZE);
	// pages = size / PAGE_SIZE;
  hdr = malloc(size);
  // TODO do something to image_ptr
  return hdr;
}

static void bpf_jit_binary_free(struct bpf_binary_header* header)
{
  // TODO (shouldn't really need this)
  // Only used for freeing
}

static struct bpf_prog* bpf_jit_blind_constants(struct bpf_prog* prog)
{
  // TODO is this ok?
  return prog;
}

static char* output_file;
static void bpf_jit_dump(u32 len, unsigned int size, int two, u32 *insns)
{
  // TODO (shouldn't really need this)
  // Used for dumping instructions?
  FILE *output_jit =
    ((strcmp(output_file, "stdout") == 0) ? stdout
     : fopen(output_file, (raw ? "wb" : "w")));
  pr_comm("bpf_jit_dump\n");
  for (int i = 0; i < size / sizeof(u32); i++)
  {
    if (raw)
    {
      // TODO bug here somewhere
      fwrite(&(insns[i]), sizeof(u32), 1, output_jit);
    }
    else
    {
      fprintf(output_jit, "%08X\n", insns[i]);
    }
  }
  fclose(output_jit);
}

static void bpf_jit_prog_release_other(struct bpf_prog *p1, struct bpf_prog *p2)
{
  // TODO (shouldn't really need this)
  // Only used for freeing
}

static void* kcalloc(size_t n, size_t size, unsigned int gfp_flags)
{
  return calloc(n, size);
}

static void* kzalloc(size_t size, gfp_t blah)
{
  return calloc(1, size);
}

static void kfree(void* ptr)
{
  free(ptr);
}

struct bpf_prog *bpf_int_jit_compile(struct bpf_prog *prog)
{
  pr_comm("Called bpf_int_jit_compile\n");
	bool tmp_blinded = false;
  bool extra_pass = false;
	struct bpf_prog *tmp, *orig_prog = prog;
	struct rv_jit_data *jit_data;
	struct rv_jit_context *ctx;
	unsigned int image_size;

  pr_err("req: %d\n", prog->jit_requested);
  /*
	if (!prog->jit_requested)
  {
    pr_err("jit_requested flag not set\n");
		return orig_prog;
  }
  */

	tmp = bpf_jit_blind_constants(prog);
	if (IS_ERR(tmp))
  {
    pr_comm("bpf_jit_blind_constants returned err\n");
		return orig_prog;
  }
	if (tmp != prog) {
    pr_comm("tmp != prog");
		tmp_blinded = true;
		prog = tmp;
	}

	if (true) {
    pr_comm("Initializing jit_data\n");
		jit_data = kzalloc(sizeof(struct rv_jit_data), GFP_KERNEL);
    pr_err("offset: %d\n", jit_data->ctx.offset);
    /*
		if (!jit_data) {
      pr_comm("kzalloc doesn't work");
			prog = orig_prog;
			goto out;
		}
		// prog->aux->jit_data = jit_data;
    */
	}

	ctx = &jit_data->ctx;

	if (ctx->offset) {
    pr_comm("ctx->offset already exists");
		extra_pass = true;
		image_size = sizeof(u32) * ctx->ninsns;
		goto skip_init_ctx;
	}
  pr_comm("ctx->offset = 0\n");

	ctx->prog = prog;
	ctx->offset = kcalloc(prog->len, sizeof(int), GFP_KERNEL);
	if (!ctx->offset) {
    pr_comm("ctx->offset not set (kcalloc)");
		prog = orig_prog;
		goto out_offset;
	}
  pr_comm("ctx->offset allocated\n");

	// First pass generates the ctx->offset, but does not emit an image.
	if (build_body(ctx, extra_pass)) {
    pr_comm("Something bad happens with build_body");
		prog = orig_prog;
		goto out_offset;
	}
  pr_comm("Body built properly on first pass, returns 0\n");
	build_prologue(ctx);
	ctx->epilogue_offset = ctx->ninsns;
	build_epilogue(ctx);

	// Allocate image, now that we know the size.
	image_size = sizeof(u32) * ctx->ninsns;
	jit_data->header = bpf_jit_binary_alloc(image_size, &jit_data->image, sizeof(u32));
	if (!jit_data->header) {
    pr_comm("jit_data->header not set properly (with bpf_jit_binary_alloc)\n");
		prog = orig_prog;
		goto out_offset;
	}
  pr_comm("jit_data->header constructed\n");

	// Second, real pass, that acutally emits the image
  // TODO this should probably be handled by bpf_jit_binary_alloc
	ctx->insns = malloc(image_size); // (u32 *)jit_data->image;
skip_init_ctx:
	ctx->ninsns = 0;

	build_prologue(ctx);
	if (build_body(ctx, extra_pass)) {
    pr_comm("Something bad happens with build_body v2");
		bpf_jit_binary_free(jit_data->header);
		prog = orig_prog;
		goto out_offset;
	}
	build_epilogue(ctx);
  pr_err("ctx->ninsns: %d\n", ctx->ninsns);

  // Looks like this is the part that actually prints out the stuff?
  // Currently set bpf_jit_enable = 0, so not taking branch
	if (bpf_jit_enable > 1)
		bpf_jit_dump(prog->len, image_size, 2, ctx->insns);

	prog->bpf_func = (void *)ctx->insns;
	prog->jited = 1;
	prog->jited_len = image_size;

  // Seems to flush instruction cache
  // Method doesn't do anything, can just ignore for now
	bpf_flush_icache(jit_data->header, ctx->insns + ctx->ninsns);

  pr_comm("Didn't take the jump\n");
	if (!prog->is_func || extra_pass) {
out_offset:
    pr_comm("Jumped to out_offset, or !prog->is_func or extra_pass\n");
		kfree(ctx->offset);
		kfree(jit_data);
    // TODO uncomment? Aux is initialized now
		// prog->aux->jit_data = NULL;
    pr_comm("Freed\n");
	}
out:
	if (tmp_blinded)
  {
		bpf_jit_prog_release_other(prog, prog == orig_prog ?
					   tmp : orig_prog);
    pr_comm("bpf_jit_prog_release_other called\n");
  }
  pr_comm("Returning\n");
	return prog;
}

static struct bpf_prog *hex_to_bpf(u64 *hex, int length)
{
  struct bpf_prog *prog = malloc(sizeof(struct bpf_prog));
  prog->insnsi = malloc(length * sizeof(struct bpf_insn));
  for (int i = 0; i < length; i++)
  {
    prog->insnsi[i].code = (u8) hex[i];
    prog->insnsi[i].dst_reg = (u8) (0x0f & (hex[i] >> 8));
    prog->insnsi[i].src_reg = (u8) (0x0f & (hex[i] >> 12));
    prog->insnsi[i].off = (s16) (hex[i] >> 16);
    prog->insnsi[i].imm = (s32) (hex[i] >> 32);
  }
  prog->len = length;
  prog->aux = malloc(sizeof(struct bpf_prog_aux));
  // TODO should this always be 0?
  prog->aux->stack_depth = 0;
  return prog;
}

static int bpf_prog_length = 0;
static u64 *read_prog(char* input_file)
{
  FILE* input = ((strcmp(input_file, "stdin") == 0) ? stdin : fopen(input_file, "r"));
  // TODO should I make this required?
  fscanf(input, "%d", &bpf_prog_length);
  u64 *prog = malloc(sizeof(u64) * bpf_prog_length);
  for (int i = 0; i < bpf_prog_length; i++)
  {
    char buf[255];
    fscanf(input, "%s", buf);
    prog[i] = strtoull(buf, NULL, 16);
    pr_err("Read input instr: %016llx\n", prog[i]);
  }
  fclose(input);
  return prog;
}

int main(int argc, char** argv) {
  if (argc < 3 || argc > 4)
  {
    pr_comm("Wrong number of arguments.");
    return 1;
  }
  // TODO allow flag to be in different place?
  // TODO allow "raw" flag
  if (argc == 4 && strcmp(argv[3], "-d") == 0)
  {
    debug = 1;
  }
  char* input_file = argv[1];
  output_file = argv[2];
  regbits = malloc(sizeof(bool) * 12);
  // prog->jit_requested = 1;
  u64 *hex = read_prog(input_file);
  struct bpf_prog *output = bpf_int_jit_compile(hex_to_bpf(hex, bpf_prog_length));
  return 0;
}
