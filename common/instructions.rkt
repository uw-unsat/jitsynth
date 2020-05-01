#lang rosette

(require rosette/lib/synthax)
(require rosette/lib/angelic)

; TODO better provides
; TODO separate these into their own files in the bpf/riscv folders
(provide (all-defined-out))

(struct old-bpf-instr (imm toff foff op) #:transparent)

(struct bpf-instr (imm off src dst func srcbit cls) #:transparent)

(define (raw->bpf-instr raw)
  (bpf-instr (extract 63 32 raw)
             (extract 31 16 raw)
             (extract 15 12 raw)
             (extract 11 8 raw)
             (extract 7 4 raw)
             (extract 3 3 raw)
             (extract 2 0 raw)))

(define (bpf-instr->raw bi)
  (concat (bpf-instr-imm bi)
          (bpf-instr-off bi)
          (bpf-instr-src bi)
          (bpf-instr-dst bi)
          (bpf-instr-func bi)
          (bpf-instr-srcbit bi)
          (bpf-instr-cls bi)))

(define (bpf-instr-op bi)
  (concat (bpf-instr-func bi)
          (bpf-instr-srcbit bi)
          (bpf-instr-cls bi)))

; TODO remove this crap
(struct riscv-instr (31-31 30-25 24-21 20-20 19-15 14-12 11-8 7-7 6-0) #:transparent)

; TODO should do some extention thing with op
; TODO want to combine u/j and s/b?
; TODO seems to be some union type error for ONLY i/s/b types (flipping func3 and src reg accidentally)
(struct riscv-rtype (func7 rs2 rs1 func3 rd op) #:transparent)
(struct riscv-utype (imm20 rd op) #:transparent)
; NOTE: Swapped func3 and rd to avoid union type
;       The "func3 then rd" order is (and should) still be maintained by genc.rkt when outputting
(struct riscv-itype (imm12 rs1 rd func3 op) #:transparent)
(struct riscv-stype (imm12 rs2 rs1 func3 op) #:transparent)
(struct riscv-btype (imm12 rs2 rs1 func3 op) #:transparent)
(struct riscv-jtype (imm20 rd op) #:transparent)

(define (raw->riscv-instr ri)
  (error "raw->riscv-instr unsupported, use decode instead"))

(define (riscv-instr->raw ri)
  (if (riscv-rtype? ri)
    (concat
      (riscv-rtype-func7 ri)
      (riscv-rtype-rs2 ri)
      (riscv-rtype-rs1 ri)
      (riscv-rtype-func3 ri)
      (riscv-rtype-rd ri)
      (riscv-rtype-op ri))
  (if (riscv-utype? ri)
    (concat
      (riscv-utype-imm20 ri)
      (riscv-utype-rd ri)
      (riscv-utype-op ri))
  (if (riscv-itype? ri)
    (concat
      (riscv-itype-imm12 ri)
      (riscv-itype-rs1 ri)
      (riscv-itype-func3 ri)
      (riscv-itype-rd ri)
      (riscv-itype-op ri))
  (if (riscv-stype? ri)
    (let ([imm (riscv-btype-imm12 ri)])
      (concat
        (extract 11 5 imm)
        (riscv-stype-rs2 ri)
        (riscv-stype-rs1 ri)
        (riscv-stype-func3 ri)
        (extract 4 0 imm)
        (riscv-stype-op ri)))
  (if (riscv-btype? ri)
    (let ([imm (riscv-btype-imm12 ri)])
      (concat
        (extract 11 11 imm)
        (extract 9 4 imm)
        (riscv-btype-rs2 ri)
        (riscv-btype-rs1 ri)
        (riscv-btype-func3 ri)
        (extract 3 0 imm)
        (extract 10 10 imm)
        (riscv-btype-op ri)))
  (if (riscv-jtype? ri)
    (let ([imm (riscv-btype-imm12 ri)])
      (concat
        (extract 19 19 imm)
        (extract 9 0 imm)
        (extract 10 10 imm)
        (extract 18 11 imm)
        (riscv-utype-rd ri)
        (riscv-utype-op ri)))
    (error "Unsupported RISC-V instruction"))))))))

; TODO put for/alls in these ops to speed stuff up?
(define (riscv-instr-op ri)
  ; (for/all ([ri ri-union])
  (if (riscv-rtype? ri) (riscv-rtype-op ri)
  (if (riscv-utype? ri) (riscv-utype-op ri)
  (if (riscv-itype? ri) (riscv-itype-op ri)
  (if (riscv-stype? ri) (riscv-stype-op ri)
  (if (riscv-btype? ri) (riscv-btype-op ri)
  (if (riscv-jtype? ri) (riscv-jtype-op ri)
    (bv 0 7))))))))
  ; (error "unsupported type"))))))))

(define (riscv-instr-dst ri)
  (if (riscv-rtype? ri) (riscv-rtype-rd ri)
  (if (riscv-utype? ri) (riscv-utype-rd ri)
  (if (riscv-itype? ri) (riscv-itype-rd ri)
  (if (riscv-stype? ri) (error "no dst for stype")
  (if (riscv-btype? ri) (error "no dst for btype")
  (if (riscv-jtype? ri) (riscv-jtype-rd ri)
  (error "riscv-instr-dst: unsupported type"))))))))

(define (riscv-instr-src1 ri)
  (if (riscv-rtype? ri) (riscv-rtype-rs1 ri)
  (if (riscv-utype? ri) (error "no src1 for utype")
  (if (riscv-itype? ri) (riscv-itype-rs1 ri)
  (if (riscv-stype? ri) (riscv-stype-rs1 ri)
  (if (riscv-btype? ri) (riscv-btype-rs1 ri)
  (if (riscv-jtype? ri) (error "no src1 for jtype")
  (error "riscv-instr-src1: unsupported type"))))))))

(define (riscv-instr-src2 ri)
  (if (riscv-rtype? ri) (riscv-rtype-rs2 ri)
  (if (riscv-utype? ri) (error "no src2 for utype")
  (if (riscv-itype? ri) (error "no src2 for itype")
  (if (riscv-stype? ri) (riscv-stype-rs2 ri)
  (if (riscv-btype? ri) (riscv-btype-rs2 ri)
  (if (riscv-jtype? ri) (error "no src2 for jtype")
  (error "riscv-instr-src2: unsupported type"))))))))

(define (riscv-instr-imm ri)
  (if (riscv-rtype? ri) (error "no imm for rtype")
  (if (riscv-utype? ri) (concat (riscv-utype-imm20 ri) (bv 0 12))
  (if (riscv-itype? ri) (sign-extend (riscv-itype-imm12 ri) (bitvector 32))
  (if (riscv-stype? ri) (sign-extend (riscv-stype-imm12 ri) (bitvector 32))
  (if (riscv-btype? ri) (sign-extend (concat (riscv-btype-imm12 ri) (bv 0 1)) (bitvector 32))
  (if (riscv-jtype? ri) (sign-extend (concat (riscv-jtype-imm20 ri) (bv 0 1)) (bitvector 32))
  (error "riscv-instr-imm: unsupported type"))))))))

; TODO this func is dumb
(define (riscv-instr-li ri)
  (if (riscv-rtype? ri) (error "no li for rtype")
  (if (riscv-utype? ri) (extract 19 8 (riscv-utype-imm20 ri))
  (if (riscv-itype? ri) (error "no li for itype")
  (if (riscv-stype? ri) (error "no li for stype")
  (if (riscv-btype? ri) (error "no li for btype")
  (if (riscv-jtype? ri) (error "no li for jtype")
  (error "riscv-instr-li: unsupported type"))))))))

; TODO somehow bv5 is possible value??
(define (riscv-instr-func3 ri)
  (if (riscv-rtype? ri) (riscv-rtype-func3 ri)
  (if (riscv-utype? ri) (error "no func3 for utype")
  (if (riscv-itype? ri) (riscv-itype-func3 ri)
  (if (riscv-stype? ri) (riscv-stype-func3 ri)
  (if (riscv-btype? ri) (riscv-btype-func3 ri)
  (if (riscv-jtype? ri) (error "no func3 for jtype")
  (error "riscv-instr-func3: unsupported type"))))))))

(define (riscv-instr-func7 ri)
  (if (riscv-rtype? ri) (riscv-rtype-func7 ri)
  (if (riscv-utype? ri) (error "no func7 for utype")
  (if (riscv-itype? ri) (error "no func7 for itype")
  (if (riscv-stype? ri) (error "no func7 for stype")
  (if (riscv-btype? ri) (error "no func7 for btype")
  (if (riscv-jtype? ri) (error "no func7 for jtype")
  (error "riscv-instr-func7: unsupported type"))))))))

; (define riscv-instr-ret
;   (raw->riscv-instr (bv #x73 32)))

(define (riscv-nop? ri)
  (and
    (riscv-itype? ri)
    (bveq (bv 0 12) (riscv-itype-imm12 ri))
    (bveq (bv 0 5) (riscv-instr-src1 ri))
    (bveq (bv 0 5) (riscv-instr-dst ri))
    (bveq (bv 0 3) (riscv-instr-func3 ri))
    (bveq (bv #x13 7) (riscv-instr-op ri))))

(define riscv-nop
  (riscv-itype (bv 0 12) (bv 0 5) (bv 0 5) (bv 0 3) (bv #x13 7)))

(define (riscv-zero? ri)
  (bveq (riscv-instr-op ri) (bv 0 7)))
