#lang rosette

(require "../common/instructions.rkt"
         "../common/case-hex.rkt"
         "../common/data-structures.rkt"
         "../common/data-utils.rkt"
         "../common/symbolic-ops.rkt"
         "../ams/machine.rkt")

(require (only-in "../common/debug.rkt" [debugln common:debugln]))

(define DEBUG #f)

(define (debugln x)
  (if DEBUG (common:debugln x) void))

(provide interpret-with-state)

(define (interpret-with-state instructions registers memory pc [max-depth (bv64 -1)])
  (interpret instructions (state registers memory pc #f) pc (bv 0 64) max-depth))

(define (interpret instructions curr-state base-pc depth max-depth)
  (if (or (bvuge depth max-depth)
          (state-return? curr-state)
          (empty? instructions)
          (ge-instruction-length? instructions (state-pc curr-state) base-pc (bv64 1)))
    curr-state
    (let* ([A (read-register (state-regs curr-state) (bv 0 8))]
           [X (read-register (state-regs curr-state) (bv 1 8))]
           [M (state-mem curr-state)]
           [pc (state-pc curr-state)]
           [should-return (state-return? curr-state)]
           [instr (read-instruction instructions pc base-pc (bv64 1))]
           ; TODO zero-extend or sign-extend? For now, zero extend b/c only forward jumps
           [toff (zero-extend (old-bpf-instr-toff instr) (bitvector 64))]
           [foff (zero-extend (old-bpf-instr-foff instr) (bitvector 64))]
           [imm (old-bpf-instr-imm instr)]
           [op-name (old-bpf-instr-op instr)]
           [jmp-cmp
             (lambda (func)
               (if (func A imm)
                 (set! pc (bvadd pc toff))
                 (set! pc (bvadd pc foff))))])
      (case op-name
        [(ld ldi) (set! A (read-memory M imm (bv64 4)))]
        ; TODO correct extend?
        [(ldh) (set! A (zero-extend (read-memory M imm (bv64 2)) (bitvector 32)))]
        [(ldb) (set! A (zero-extend (read-memory M imm (bv64 1)) (bitvector 32)))]
        [(ldx ldxi) (set! X (read-memory M imm (bv64 4)))]
        ; TODO correct extend?
        [(ldxb) (set! X (zero-extend (read-memory M imm (bv64 1)) (bitvector 32)))]
        [(st) (set! M (write-memory M imm (bv64 4) A))]
        [(stx) (set! M (write-memory M imm (bv64 4) X))]
        [(jmp ja) (jmp-cmp (thunk* #t))]
        [(jeq) (jmp-cmp bveq)]
        [(jneq jne) (jmp-cmp (lambda (a b) (not (bveq a b))))]
        [(jlt) (jmp-cmp bvslt)]
        [(jle) (jmp-cmp bvsle)]
        [(jgt) (jmp-cmp bvsgt)]
        [(jge) (jmp-cmp bvsge)]
        ; TODO check eBPF jset impl
        [(jset) (jmp-cmp
                  (lambda (a b)
                    (not (bveq (bvand imm A) (bv32 0)))))]
        [(add) (set! A (bvadd A imm))]
        [(sub) (set! A (bvsub A imm))]
        [(mul) (set! A (smul32 A imm))]
        [(div) (set! A (sdiv32 A imm))]
        [(mod) (set! A (smod32 A imm))]
        ; [(neg) (set! A (bvneg A))] ; NOTE: this isnt in the official instruction set
        [(and) (set! A (bvand A imm))]
        [(or)  (set! A (bvor A imm))]
        [(xor) (set! A (bvxor A imm))]
        [(lsh)
         ; TODO should I really have these asserts here?
         (assert (bvult imm (bv #b100000 32)))
         (set! A (bvshl A imm))]
        [(rsh)
         (assert (bvult imm (bv #b100000 32)))
         (set! A (bvlshr A imm))]
        [(tax) (set! X A)]
        [(txa) (set! A X)]
        [(ret) (set! should-return #t)]
        [else (error "instruction not recognized")])
      ; Write everything (X and A in regs)
      (interpret
        instructions
        (state 
          ; TODO is this the right order?
          (write-register
            (write-register
              (state-regs curr-state)
              (bv 0 8) A)
            (bv 1 8) X)
          ; TODO: Is this extra 1 here right?
          M (bvadd pc (bv64 1)) should-return)
        base-pc (bvadd (bv64 1) depth) max-depth))))
