#lang rosette

(require "tostr.rkt"
         "../common/data-structures.rkt"
         "../common/data-utils.rkt"
         "../ams/machine.rkt"
         "../common/case-hex.rkt"
         "../common/instructions.rkt"
         "../common/symbolic-ops.rkt")

(require (only-in "../common/debug.rkt" [debugln common:debugln]))

(provide interpret-with-init-state)
(provide interpret-with-state)
(provide pc-add)

(define DEBUG #f)

(define (debugln x)
  (if DEBUG (common:debugln x) void))

; TODO probably a bug in here (for when values are symbolic)
(define (simplify-bvadd x y)
  ; (debugln (~a "SBVADD: " x ", " y))
  (match* (x y)
    [((bv c1 l) (expression (== bvadd) (bv c2 l) n))
     (begin
       (bvadd (bvadd (bv c1 l) (bv c2 l)) n))]
    [(a b)
     (begin
       (bvadd a b))]))

; TODO try to put this for/all in other places as well
; TODO probably a bug in here (for when values are symbolic)
; Either want to push all the conditionals to the top or bottom (probably top)
(define (pc-add off my-pc)
  ; (debugln "\nPC ADD")
  ; (debugln (~a "Asserts: " (asserts)))
  ; (debugln (~a "Off & PC: " off ", " my-pc))
  ; (debugln (pc))
  (define res ; (simplify-bvadd off pc))
    (for/all ([p my-pc #:exhaustive])
      (begin
        ; (debugln (~a "Subvalue: " p))
        (simplify-bvadd off p))))
  ; (debugln (~a "Result: " res))
  ; (debugln (~a "Asserts: " (asserts)))
  ; (debugln "PC ADD DONE\n")
  res)

(define (apply-arith-64 ri rvals)
  (let* ([func3 (riscv-instr-func3 ri)]
         [func7 (riscv-instr-func7 ri)]
         [src1 (riscv-instr-src1 ri)]
         [src2 (riscv-instr-src2 ri)]
         [dst (riscv-instr-dst ri)]
         [func (case-hex func3 3
                         [(0) (case-hex func7 7
                                  [(0) bvadd]
                                  [(32) bvsub]
                                  ; TODO change back?
                                  [(1) smul])]
                         [(1)
                          (lambda (a b)
                            (bvshl a (bvand (bv64 #x3f) b)))]
                         [(2) (error "slt not implemented")]
                         [(3) (error "sltu not implemented")]
                         [(4) bvxor] ; NOTE: May need another case for signed division
                         [(5) (case-hex func7 7
                                  [(0)
                                   (lambda (a b)
                                     (bvlshr a (bvand (bv64 #x3f) b)))]
                                  [(1) sdiv] ; bvudiv
                                  [(32)
                                   (lambda (a b)
                                     (bvashr a (bvand (bv64 #x3f) b)))])] ; arith right shift if func7 is 32
                         [(6) bvor]
                         [(7) (case-hex func7 7
                                        [(0) bvand]
                                        [(1) smod])] ; bvurem
                         )])
    (write-register rvals
                 dst
                 (apply func
                        (list
                         (read-register rvals src1)
                         (read-register rvals src2))))))

(define (apply-arith-32 ri rvals)
  (let* ([func3 (riscv-instr-func3 ri)]
         [func7 (riscv-instr-func7 ri)]
         [src1 (riscv-instr-src1 ri)]
         [src2 (riscv-instr-src2 ri)]
         [mask
           (lambda (val)
             (extract 31 0 val))]
         [extend
           (lambda (val)
             (sign-extend val (bitvector 64)))]
         [src1-val (mask (read-register rvals src1))]
         [src2-val (mask (read-register rvals src2))]
         [dst (riscv-instr-dst ri)]
         [func (case-hex func3 3
                         [(0) (case-hex func7 7
                                  [(0) bvadd]
                                  [(32) bvsub]
                                  ; TODO change back?
                                  [(1) smul32])]
                         [(1)
                          (lambda (a b)
                            (bvshl a (bvand (bv32 #x1f) b)))]
                         [(2) (error "slt not implemented")]
                         [(3) (error "sltu not implemented")]
                         ; [(4) bvxor] ; Need cases if want signed division
                         [(5) (case-hex func7 7
                                  [(0)
                                   (lambda (a b)
                                     (bvlshr a (bvand (bv32 #x1f) b)))]
                                  [(1) sdiv32] ; bvudiv
                                  ; NOTE: This is the only op that doesn't work by masking
                                  [(32)
                                   (lambda (a b)
                                     (bvashr a (bvand (bv32 #x1f) b)))])] ; arith right shift if func7 is 32
                                   ; (lambda (a b)
                                   ;   (let ([a32 (sign-extend (extract 31 0 a) (bitvector 64))])
                                   ;     (bvashr a32 b)))])]
                         ; [(6) bvor]
                         [(7) (case-hex func7 7
                         ;                [(0) bvand]
                                        [(1) smod32])] ; bvurem
                         )])
    (write-register
      rvals dst
      (extend (apply func (list src1-val src2-val))))))

(define (apply-imm-arith-64 ri rvals)
  (let* ([func3 (riscv-instr-func3 ri)]
         [imm (riscv-instr-imm ri)]
         [src (riscv-instr-src1 ri)]
         [src-val (read-register rvals src)]
         [imm-val (sign-extend imm (bitvector 64))]
         [dst (riscv-instr-dst ri)]
         [func (case-hex func3 3
                         [(0) bvadd]
                         [(1)
                          (assert (bvule imm-val (bv #b111111 64)))
                          bvshl]
                         [(2) (error "slti not implemented")]
                         [(3) (error "sltiu not implemented")]
                         [(4) bvxor]
                         [(5)
                          (set! imm-val (bvand imm-val (bv #xfffffffffffffbff 64)))
                          (assert (bvule imm-val (bv #b111111 64)))
                          (case-hex (extract 10 10 imm) 1
                            [(0) bvlshr]
                            [(1) bvashr])]
                         [(6) bvor]
                         [(7) bvand]
                         )]
         [res (apply func (list src-val imm-val))])
    (write-register rvals dst res)))

(define (apply-imm-arith-32 ri rvals)
  (let* ([func3 (riscv-instr-func3 ri)]
         [imm (riscv-instr-imm ri)]
         [src (riscv-instr-src1 ri)]
         [dst (riscv-instr-dst ri)]
         [mask
           (lambda (val)
             (extract 31 0 val))]
         [extend
           (lambda (val)
             (sign-extend val (bitvector 64)))]
         [src-val (mask (read-register rvals src))]
         [imm-val (sign-extend imm (bitvector 32))]
         [func (case-hex func3 3
                         [(0) bvadd]
                         ; TODO can I avoid these extracts?
                         [(1)
                          (assert (bvule imm-val (bv #b11111 32)))
                          bvshl]
                         [(2) (error "sltiw not allowed")]
                         [(3) (error "sltiuw not allowed")]
                         ; [(4) bvxor]
                         [(5)
                          (set! imm-val (bvand imm-val (bv #xfffffbff 32)))
                          (assert (bvule imm-val (bv #b111111 32)))
                          (case-hex (extract 10 10 imm) 1
                            [(0) bvlshr]
                            [(1) bvashr])]
                         [(6) (error "oriw not allowed")]
                         [(7) (error "andiw not allowed")]
                         )])
    (write-register
      rvals dst
      (extend (apply func (list src-val imm-val))))))

(define (apply-load ri rvals memory)
  (let* ([width (riscv-instr-func3 ri)]
         [base (riscv-instr-src1 ri)]
         [base-val (read-register rvals base)]
         [dst (riscv-instr-dst ri)]
         ; TODO are these sign extends a problem?
         [offset (sign-extend (riscv-instr-imm ri) (bitvector 64))]
         [num-bytes
           ; TODO make signed vs unsigned
           (case-hex width 3
             [(0) (bv 1 16)]
             [(1) (bv 2 16)]
             [(2) (bv 4 16)]
             [(3) (bv 8 16)]
             [(4) (bv 1 16)]
             [(5) (bv 2 16)]
             [(6) (bv 4 16)])]
         [index (bvadd offset base-val)]
         [zext?
           (case-hex width 3
             [(0) #f]
             [(1) #f]
             [(2) #f]
             [(3) #t]
             [(4) #t]
             [(5) #t]
             [(6) #t])]
         ; TODO should this be zero-extend?
         [memval (apply (if zext? zero-extend sign-extend)
                        (list (read-memory memory index num-bytes) (bitvector 64)))])
    (write-register rvals dst memval)))

(define (apply-store ri rvals memory)
  (let* ([width (riscv-instr-func3 ri)]
         [src (riscv-instr-src2 ri)]
         [base (riscv-instr-src1 ri)]
         [base-val (read-register rvals base)]
         ; TODO make this a convenience function
         [offset (sign-extend (riscv-instr-imm ri) (bitvector 64))]
         [num-bytes (bvshl (bv 1 16) (zero-extend width (bitvector 16)))]
         [value (read-register rvals src)]
         [index (bvadd offset base-val)])
    (write-memory memory index num-bytes value)))

; TODO semantics here might be wrong, may need to first update PC
(define (apply-auipc ri rvals pc)
  (let* ([imm (riscv-instr-imm ri)]
         [dst (riscv-instr-dst ri)]
         ; TODO not sure if sign extend is correct here
         [res (bvadd pc (sign-extend imm (bitvector 64)))])
    (write-register rvals dst res)))

; NOTE: The result is sign extended always
(define (apply-lui ri rvals)
  (let* ([imm (riscv-instr-imm ri)]
         [dst (riscv-instr-dst ri)])
    (write-register rvals dst (sign-extend imm (bitvector 64)))))

(define (apply-slui ri rvals)
  (let* ([imm (riscv-instr-imm ri)]
         [dst (riscv-instr-dst ri)])
    (write-register rvals dst (sign-extend imm (bitvector 64)))))

(define (apply-lli ri rvals)
  (let* ([imm (riscv-instr-li ri)]
         [dst (riscv-instr-dst ri)])
    (write-register rvals dst (concat (extract 63 12 (read-register rvals dst)) imm))))

(define (jump-cond-to-pc ri rvals pc)
  (let* ([funct (riscv-instr-func3 ri)]
        [rs2 (riscv-instr-src2 ri)]
        [rs1 (riscv-instr-src1 ri)]
        [offset (sign-extend (riscv-instr-imm ri) (bitvector 64))]
        [left (read-register rvals rs1)]
        [right (read-register rvals rs2)]
        [comparator (case-hex funct 3
                              [(0) bveq] ; beq
                              [(1) (lambda (a b) (not (bveq a b)))] ; bne
                              [(4) bvslt] ; blt
                              [(5) bvsge] ; bge
                              [(6) bvult] ; bltu
                              [(7) bvuge])]) ; bgeu
    ; TODO should also add a test for alignment here
    (if (apply comparator (list left right))
      (begin
        (debugln "Did BRANCH")
        (pc-add offset pc))
      (pc-add (bv 4 64) pc))))

(define (jump-to-pc ri rvals pc)
  (let* ([dst (riscv-instr-dst ri)] ; assume this is 0
         [offset (sign-extend (riscv-instr-imm ri) (bitvector 64))]
         ; TODO should also add a test for alignment here
         ; TODO I think that I shouldn't be adding 4 here, not sure
         [addr (pc-add offset pc)])
    (list
      addr
      (write-register rvals dst addr))))

; TODO should also add a test for alignment here
; TODO is there a more efficient way to implement this?
; Return a pair of PC and rvals
(define (jump-and-load-reg ri rvals)
  (let* ([dst (riscv-instr-dst ri)]
         [offset (sign-extend (riscv-instr-imm ri) (bitvector 64))]
         [src1 (riscv-instr-src1 ri)]
         [base (read-register rvals src1)]
         ; NOTE: for/all b/c PC operation here
         [addr
           (for/all ([p base #:exhaustive])
             (bvand (bvadd offset p) (bv -2 64)))])
    (debugln "Did JALR")
    (list
      addr
      (write-register rvals dst addr))))

(define (interpret-with-init-state instructions)
  (interpret-with-state instructions (make-zerod-registers 32) (make-zerod-memory 100) (bv 0 64)))

; TODO make this accept an actual state
(define (interpret-with-state instructions registers memory pc [max-depth (bv -1 64)])
  (interpret instructions
             (make-state
               ; TODO I think hardwire is broken for some reason
              (hardwire-register registers (bv 0 5) (bv 0 64))
              memory
              pc)
             pc
             (bv 0 64)
             max-depth))

; NOTE: Base PC is already shifted
; TODO could speed up symbolic execution by using the fact that there are no loops
;      (and thus cannot jump to a PC that you've already seen)
(define (interpret instructions curr-state base-pc depth max-depth)
  (debugln (~a "Interp PC: " (state-pc curr-state)))
  (if (or (bvuge depth max-depth)
          (empty? instructions)
          (state-return? curr-state)
          ; NOTE: Also added check for jumping before the instruction here
          (pc-out-of-bounds? instructions (state-pc curr-state) base-pc (bv 4 64)))
      curr-state
      (let* ([rvals (state-regs curr-state)]
             [memory (state-mem curr-state)]
             [pc (state-pc curr-state)]
             [new-pc (pc-add (bv 4 64) pc)]
             [ri (read-instruction instructions pc base-pc (bv 4 64))]
             [should-return #f]
             [opcode (riscv-instr-op ri)])
        (begin
          ; (if (union? opcode)
          ;   (debugln (~a "OP Union: " (union-contents opcode)))
          ;   (debugln (~a "OP: " opcode)))
          (debugln (~a "Instr struct: " ri))
          (debugln (~a "Instr: " (tostr-instr ri)))
          (case-hex opcode 7
                    ; 32-bit arith
                    [(#x3b) (set! rvals (apply-arith-32 ri rvals))]

                    ; 32-bit imm arith
                    [(#x1b) (set! rvals (apply-imm-arith-32 ri rvals))]

                    ; 64-bit arith
                    [(#x33) (set! rvals (apply-arith-64 ri rvals))]

                    ; 64-bit imm arith
                    [(#x13) (set! rvals (apply-imm-arith-64 ri rvals))]

                    ; loads
                    [(#x03) (set! rvals (apply-load ri rvals memory))]

                    ; stores
                    [(#x23) (set! memory (apply-store ri rvals memory))]

                    ; uncond jump (jal)
                    [(#x6f)
                     (let ([res (jump-to-pc ri rvals pc)])
                       (set! new-pc (first res))
                       (set! rvals (second res)))]

                    ; cond jumps
                    [(#x63) (set! new-pc (jump-cond-to-pc ri rvals pc))]

                    ; lui
                    [(#x37) (set! rvals (apply-lui ri rvals))]

                    ; rets
                    [(#x73) (set! should-return #t)]

                    ; jalr
                    [(#x67)
                     (let ([res (jump-and-load-reg ri rvals)])
                       (set! new-pc (first res))
                       (set! rvals (second res)))]

                    ; auipc
                    [(#x17) (set! rvals (apply-auipc ri rvals pc))]

                    ; Padding?
                    [(0) #f]

                    ; special instructions (that I made up)
                    ; this is to shorten the size of (synthesized) assembler,
                    ; though this domain knowledge shouldn't be encoded in the interpreter
                    ; signed lui
                    [(#x7f) (set! rvals (apply-slui ri rvals))]
                    ; lli (load lower immediate)
                    [(#x7e) (set! rvals (apply-lli ri rvals))])
          ; (assert (or (bvugt new-pc pc) (bvult new-pc base-pc)))
          (interpret instructions (state rvals memory new-pc should-return)
                     ; NOTE: Replaced base-pc with pc (old) to ensure that jumps are always forward
                     base-pc (bvadd (bv 1 64) depth) max-depth)))))
