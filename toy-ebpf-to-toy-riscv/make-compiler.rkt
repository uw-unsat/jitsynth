#lang rosette

(require 
  "stp.rkt"
  "../toy-riscv/interpreter.rkt"
  "../toy-riscv/machine-instance.rkt"
  "../common/case-hex.rkt"
  "../common/data-utils.rkt"
  "../common/data-structures.rkt"
  "../common/options.rkt"
  "../ams/machine.rkt")
(require (except-in "../toy-ebpf/interpreter.rkt" interpret))
(require rosette/lib/angelic)

(define stp (make-stp))

(define ebpf-iws (get-iwss (src/tgt-source stp)))
(define riscv-iws (get-iwss (src/tgt-target stp)))

(define dst (symbv 4))
(define src (symbv 4))
(define off (symbv 16))
(define imm (symbv 32))
(define src-insn (ebpf-insn 'addi32 dst src off imm))

; This is an intermediate things synthesized
; I can also make a query for this if needed
(define modif-imm (bvadd imm (bvshl (bvand imm (bv32 #x800)) (bv32 1))))
(define load
  (list
    (rv-insn
      'lui
      (bv #b111 5)
      (bv 0 5)
      (bv 0 5)
      (extract 31 12 modif-imm))
    (rv-insn
      'addiw
      (bv #b111 5)
      (bv #b111 5)
      (bv 0 5)
      (extract 11 0 modif-imm))))

(define regmap
  (lambda (index)
    (case-hex index 4
      [(0) (bv 15 5)]
      [(1) (bv 10 8)]
      [(2) (bv 11 5)]
      [(3) (bv 12 5)]
      [(4) (bv 13 5)]
      [(5) (bv 14 5)]
      [(6) (bv 9 5)]
      [(7) (bv 18 5)]
      [(8) (bv 19 5)]
      [(9) (bv 20 5)]
      [(10) (bv 21 5)]
      [(11) (bv 5 5)])))

#|
    (choose* (symbv 5) (regmap dst))
    (choose* (symbv 5) (regmap dst))
    (choose* (symbv 5) (regmap dst))
    (symbv (if (equal? name 'lui) 20 12))))
|#

(define (insn-sketch-helper
          name r1 r2 r3 imm12 imm20)
  (rv-insn name r1 r2 r3 (if (equal? name 'lui) imm20 imm12)))

(define (opt-insn-sketch)
  (let ([names '(lui addiw add slli srli)]
        [r1 (choose* (symbv 5) (regmap dst))]
        [r2 (choose* (symbv 5) (regmap dst))]
        [r3 (choose* (symbv 5) (regmap dst))]
        [imm12 (symbv 12)]
        [imm20 (symbv 20)])
    (apply choose*
      (map
        (lambda (n)
          (insn-sketch-helper
            n r1 r2 r3 imm12 imm20))
        names))))

(define (basic-insn-sketch immval)
  (let* ([names '(lui addiw add slli srli ld sd)]
         [r1 (choose* (symbv 5) (regmap dst) (regmap src))]
         [r2 (choose* (symbv 5) (regmap dst) (regmap src))]
         [r3 (choose* (symbv 5) (regmap dst) (regmap src))]
         [symimmval (symbv 20)]
         [imm12 (extract 11 0 (choose* symimmval immval))]
         [imm20 (choose* symimmval (extract 31 12 immval))])
    (apply choose*
      (map
        (lambda (n)
          (insn-sketch-helper
            n r1 r2 r3 imm12 imm20))
        names))))

(define (opt-sketch)
  (append load (build-list 3 (thunk* (opt-insn-sketch)))))

(define (basic-sketch)
  (let ([immval (symbv-func-apply 48 32 (concat off imm))])
    (build-list 5 (thunk* (basic-insn-sketch immval)))))

(define (mid-sketch)
  (let ([immval (symbv-func-apply 48 32 (concat off imm))])
    (append load (build-list 3 (thunk* (basic-insn-sketch immval))))))

(define tgt-start-state (machine-default-state (src/tgt-target stp)))
(define src-start-state (t2s-state stp tgt-start-state))

(define (query sketch)
  (synthesize
    #:forall (list src-insn tgt-start-state)
    #:assume (ebpf-iws (list src-insn) src-start-state (bv64 1))
    #:guarantee
    (let* ([src-state (ebpf-iws (list src-insn) src-start-state (bv64 1))]
           [tgt-state (riscv-iws sketch tgt-start-state (bv64 (length sketch)))])
      (assert
        (bveq 
          (read-register
            (state-regs tgt-state)
            (regmap dst))
          (read-register
            (state-regs src-state)
            dst))))))

(define skch (basic-sketch))
; (define skch (opt-sketch))
; (define skch (mid-sketch))
(define mdl
  (time
    (complete-solution
      (query skch)
      (remove* (symbolics src-insn) (symbolics skch)))))
(for ([i 5])
  (displayln (list-ref (evaluate skch mdl) i)))
