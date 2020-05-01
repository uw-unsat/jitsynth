#lang rosette

; TODO rename this to machine-factory

; TODO would be nice to serialize machines
(require "../ams/machine.rkt" "../ams/machine-loader.rkt"
         "../common/instructions.rkt" "../common/data-utils.rkt"
         "../common/data-structures.rkt")
(require (only-in "../riscv/interpreter.rkt" [interpret-with-state riscv:interpret-with-state]))

(provide param-values riscv-constructor make-riscv-machine)

; return int (bv length) or 'reg (mem not allowed in instr, not allowed direct access)
; just consider target instructions
(define (param-type param-name)
  (case param-name
    [(src1 src2 dst) 'reg]
    [(func3 func7 op) 'op]
    ['imm20 20]
    ['imm12 12]))

(define (param-am-type param-name)
  (let ([type (param-type param-name)])
    (if (integer? type) 'value type)))

; return symbolic bitvector of correct length for int (bv)
; return choice between all target regs (constant) and mapping of source instr regs to target regs for 'reg
; TODO will need to label regs as regs so scratch-reg vs full-reg decision can be made on gensketch side
; TODO need to supply regmap and srcregs
(define (type-vals pt)
  (case pt
    ['reg (symbv 5)]
    ; TODO allow for srcregs?
    #|(apply
        choose*
        (flatten-list
          (riscv-scratch-regs)
          (map regmap srcregs)))]|#
  [else (symbv pt)]))

(define (param-values name op)
  (case name
    [(func3)
     (case op
       [(add add32
             sub sub32
             mul mul32
             addi addi32
             lb sb
             beq jalr) (bv 0 3)]
       [(shl shl32
             shli shli32
             lh sh
             bne) (bv 1 3)]
       [(lw sw) (bv 2 3)]
       [(ld sd) (bv 3 3)]
       [(xor xor32
             xori
             blt
             lbu) (bv 4 3)]
       [(lshr lshr32
              ashr ashr32
              lshri lshri32
              ashri ashri32
              divu divu32
              bge
              lhu) (bv 5 3)]
       [(or or32
            ori ori32
            bltu
            lwu) (bv 6 3)]
       [(and and32
             mod mod32
             andi
             bgeu) (bv 7 3)]
       [else (bv 0 3)])]
    [(func7)
     (case op
       [(add and or xor shl lshr
             add32 and32 or32 xor32 shl32 lshr32)
        (bv 0 7)]
       [(sub ashr
             sub32 ashr32)
        (bv #b0100000 7)]
       [(mul divu mod
             mul32 divu32 mod32)
        (bv 1 7)]
       [else (bv 0 7)])]
    [(op)
     (case op
       [(add sub mul divu mod and or xor shl lshr ashr)
        (bv #x33 7)]
       [(addi andi ori xori shli lshri ashri)
        (bv #x13 7)]
       [(add32 sub32 mul32 divu32 mod32 and32 or32 xor32 shl32 lshr32 ashr32)
        (bv #x3b 7)]
       [(addi32 shli32 lshri32 ashri32)
        (bv #x1b 7)]
       [(lb lh lw ld lbu lhu lwu)
        (bv #x03 7)]
       [(sb sh sw sd)
        (bv #x23 7)]
       [(jal)
        (bv #x6f 7)]
       [(beq bne blt bge bltu bgeu)
        (bv #x63 7)]
       [(jalr)
        (bv #x67 7)]
       [(auipc)
        (bv #x17 7)]
       [(lui)
        (bv #x37 7)]
       [else (bv 0 7)])]
    ; src1 src2 dst imm20 imm12
    [else (type-vals (param-type name))]))

(define (riscv-constructor op)
  (case op
    [(add sub mul divu mod and or xor shl lshr ashr
          add32 sub32 mul32 divu32 mod32 and32 or32 xor32 shl32 lshr32 ashr32)
     riscv-rtype]
    [(addi andi ori xori shli lshri ashri
           addi32 shli32 lshri32 ashri32
           lb lh lw ld lbu lhu lwu jalr)
     riscv-itype]
    [(sb sh sw sd)
     riscv-stype]
    [(jal)
     riscv-jtype]
    [(lui auipc)
     riscv-utype]
    [(beq bne blt bge bltu bgeu)
     riscv-btype]))

; TODO this structure causes union types in the instruction
(define (riscv-instr-params op)
  (case op
    [(add sub mul divu mod and or xor shl lshr ashr
          add32 sub32 mul32 divu32 mod32 and32 or32 xor32 shl32 lshr32 ashr32)
     (list 'func7 'src2 'src1 'func3 'dst 'op)]
    [(addi andi ori xori shli lshri ashri
           addi32 shli32 lshri32 ashri32
           lb lh lw ld lbu lhu lwu jalr)
     (list 'imm12 'src1 'dst 'func3 'op)]
    [(sb sh sw sd
         beq bne blt bge bltu bgeu)
     (list 'imm12 'src2 'src1 'func3 'op)]
    [(jal lui auipc)
     (list 'imm20 'dst 'op)]))

; TODO should I do this as a function from param name to bitlen instead?
(define (riscv-instr-param-bitlens op)
  (case op
    [(add sub mul divu mod and or xor shl lshr ashr
          add32 sub32 mul32 divu32 mod32 and32 or32 xor32 shl32 lshr32 ashr32)
     (list 7 5 5 3 5 7)]
    [(addi andi ori xori shli lshri ashri
           addi32 shli32 lshri32 ashri32
           lb lh lw ld lbu lhu lwu jalr)
     (list 12 5 5 3 7)]
    [(sb sh sw sd
         beq bne blt bge bltu bgeu)
     (list 12 5 5 3 7)]
    [(jal lui auipc)
     (list 20 5 7)]))

(define (riscv-op-names)
  '(add sub mul divu mod and or xor shl lshr ashr
        addi andi ori xori shli lshri ashri
        add32 sub32 mul32 divu32 mod32 #| and32 or32 xor32 |# shl32 lshr32 ashr32
        addi32 shli32 lshri32 ashri32
        lb lh lw ld lbu lhu lwu sb sh sw sd
        jal beq bne blt bge bltu bgeu lui jalr auipc))

(define (make-riscv-machine
          [riscv-names (riscv-op-names)])
  (define riscv-instructions
    (for/list ([name riscv-names])
      (let ([params (riscv-instr-params name)]
            [param-bitlens (riscv-instr-param-bitlens name)])
        (make-instruction
          name
          (riscv-constructor name)
          (make-list (length params) #f) ; TODO do I even need this?
          (for/list ([param params])
            (thunk (param-values param name)))
          (map param-am-type params)
          param-bitlens
          (make-list (length params) #f) ; Nothing is cachable (though this is technically wrong)
          params))))

  (define (instr->getters ri)
    (cond
      [(riscv-rtype? ri)
       (list riscv-rtype-func7 riscv-rtype-rs2 riscv-rtype-rs1
             riscv-rtype-func3 riscv-rtype-rd riscv-rtype-op)]
      [(riscv-utype? ri)
       (list riscv-utype-imm20 riscv-utype-rd riscv-utype-op)]
      [(riscv-itype? ri)
       (list riscv-itype-imm12 riscv-itype-rs1
             riscv-itype-func3 riscv-itype-rd riscv-itype-op)]
      [(riscv-stype? ri)
       (list riscv-stype-imm12 riscv-stype-rs2 riscv-stype-rs1
             riscv-stype-func3 riscv-stype-op)]
      [(riscv-btype? ri)
       (list riscv-btype-imm12 riscv-btype-rs2 riscv-btype-rs1
             riscv-btype-func3 riscv-btype-op)]
      [(riscv-jtype? ri)
       (list riscv-jtype-imm20 riscv-jtype-rd riscv-jtype-op)]
      [else (error "instr->getters riscv problem")]))


  (define riscv-regs (make-registers 32))
  ; TODO make bigger?
  (define riscv-mem (make-memory 128))
  (define-symbolic riscv-pc (bitvector 64))
  (define start-state
    (make-state
      (write-register
        riscv-regs
        (bv 0 5)
        (bv 0 64))
      riscv-mem
      riscv-pc))
  (define-values (riscv-machine); riscv-machine-asserts)
    ; TODO (with-asserts
    (make-machine
      start-state
      #:state-asmt-func
      (lambda (st)
        (assert (bvult (state-pc st) (bv #x8000000000000000 64))))
      #:state-invar-func
      (lambda (st)
        (assert (bveq (bv 0 64) (bvand (state-pc st) (bv 3 64)))))
      riscv:interpret-with-state
      riscv-instructions
      riscv-nop
      instr->getters
      "riscv/rwset-cache"))

  riscv-machine)
