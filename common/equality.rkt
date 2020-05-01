#lang rosette

(require "../bpf/parser.rkt")
(require "../bpf/op.rkt")
(require "../bpf/transformer.rkt")
(require "../bpf/assembler.rkt")
(require (only-in "../bpf/interpreter.rkt" [interpret-with-state interpret-bpf-with-state]))
(require (only-in "../riscv/interpreter.rkt" [interpret-with-state interpret-riscv-with-state]))
(require "../common/data-structures.rkt")
(require "../common/case-hex.rkt")
(require "../common/state.rkt")
(require (only-in "../common/debug.rkt" [debugln common:debugln]))
(require rosette/lib/angelic)
(require rosette/lib/synthax)

(provide (all-defined-out))

(define DEBUG #f)

(define (debugln x)
  (if DEBUG (common:debugln x) void))

(define (bv8 i) (bv i 8))
(define (bv64 i) (bv i 64))

(define (riscv-regs->bpf regs)
  (make-regs-from-existing
   regs
   (lambda (index)
     (case-hex index 8
    [(0) (bv 15 8)]
    [(1) (bv 10 8)]
    [(2) (bv 11 8)]
    [(3) (bv 12 8)]
    [(4) (bv 13 8)]
    [(5) (bv 14 8)]
    [(6) (bv 9 8)]
    [(7) (bv 18 8)]
    [(8) (bv 19 8)]
    [(9) (bv 20 8)]
    [(10) (bv 21 8)]
    [(11) (bv 5 8)]))
   12))

; NOTE: This implementation lacks generality.
;       Instead, keep track of singularly written BPF reg
;       For that reg, check that RISC-V reg is equal
;       Then also add a check that all other regs are the same
;       if read from the base (easy to simplify early)
(define (regs-equal? bpf-regs riscv-regs)
  (let* ([riscv/bpf-regs (riscv-regs->bpf riscv-regs)])
    (foldl
     (lambda (index acc)
       (and (bveq (read-register bpf-regs index)
                  (read-register riscv/bpf-regs index))
            acc))
     #t
     (build-list 12 bv8))))

; NOTE: This implementation lacks generality.
;       Can improve with forall query.
(define (mem-equal? bpf-mem riscv-mem)
  (foldl
   (lambda (index acc)
     (and (bveq (read-memory bpf-mem index (bv 1 1))
                (read-memory riscv-mem index (bv 1 1)))
          acc))
   #t
   (build-list 16 bv8)))

(define (symbv l)
  (define-symbolic* sym (bitvector l))
  l)

(define (pc-equal-reg? bpf-pc riscv-regs pcfact)
  (let ([desired-pc (bvmul bpf-pc (bvmul (bv 4 64) pcfact))])
    ; TODO replace (bv 7 5) w/ (symbv 5) (or more practically, scratch reg)
    (bveq desired-pc (read-register riscv-regs (bv 7 8)))))

; Assume all 64-bit
(define (pc-equal? bpf-pc riscv-pc pcfact)
  (let ([desired-pc (bvmul bpf-pc (bvmul (bv 4 64) pcfact))])
  ; NOTE: nonlinearity may cause slowdown
  ; TODO take in 4 as a parameter? Probably not
    (bveq desired-pc riscv-pc)))

; NOTE: May want to provide option for ignoring parts of state
(define (state-equal? bpf-state riscv-state base-pc pcfact enabled)
  (and (implies
         (first enabled)
         (regs-equal? (state-rvals bpf-state) (state-rvals riscv-state)))
       (implies
         (second enabled)
         (mem-equal? (state-memory bpf-state) (state-memory riscv-state)))
       (implies
         (third enabled)
         (or
           ; Want to say that either PCs are equal at the end OR
           (pc-equal? (state-pc bpf-state) (state-pc riscv-state) pcfact)))))
#|
; TODO
           (and
             (pc-equal?
               (bvadd (bv 1 64) base-pc)
               ; TODO replace +4 here with +(4*storelen)
               (bvadd (bv 4 64) (state-pc riscv-state))
               pcfact)
             (pc-equal-reg? (state-pc bpf-state) (state-rvals riscv-state) pcfact))))))
|#

; TODO rename pcfact to "rv-proglen"
(define (has-same-behavior-bool
          bpf-prog riscv-prog
          riscv-regs memory pc
          max-depth pcfact
          [enabled (list #t #t #t)])
  (let* ([riscv-interp-result
           (interpret-riscv-with-state
             riscv-prog riscv-regs memory (bvmul pc (bvmul (bv 4 64) pcfact)) max-depth)]
         ; NOTE: Assuming BPF program has length 1
         [bpf-interp-result
           (interpret-bpf-with-state
             bpf-prog (riscv-regs->bpf riscv-regs) memory pc (bv 1 64))])
    (debugln "Done with symbolic execution.")
    (state-equal?
      bpf-interp-result
      riscv-interp-result
      pc
      pcfact enabled)))

(define (has-same-behavior?
          bpf-prog riscv-prog
          riscv-regs memory pc
          max-depth pcfact [enabled (list #t #t #t)])
  (assert (has-same-behavior-bool
            bpf-prog riscv-prog
            riscv-regs memory pc
            max-depth pcfact enabled)))
