#lang rosette

(require "../ams/machine.rkt"
         "../ams/machine-loader.rkt"
         "../common/instructions.rkt"
         "../common/data-utils.rkt"
         "../common/data-structures.rkt"
         "interpreter.rkt")

(require rosette/lib/angelic)

(provide make-old-bpf-machine
         old-bpf-op-names)

(define (old-bpf-op-names)
  '(add sub mul div mod and or xor
        lsh rsh tax txa ret
        ld ldi ldh ldb ldx ldxi ldxb
        st stx
        jmp ja
        jeq jneq jne jlt jle jgt jge jset))

(define (make-old-bpf-machine
          [op-names (old-bpf-op-names)]
          #:rwset-cache [rwset-cache "old-bpf/rwset-cache"])
  (define (make-imm name)
    (case name
      [(tax txa ret jmp ja) (bvn0l 32)]
      [(ld ldi ldh ldb ldx ldxi ldxb st stx)
       (apply choose* (map bv32 (build-list 16 values)))]
      [else (symbv 32)]))

  (define (make-toff name)
    (case name
      [(ld ldi ldh ldb ldx ldxi ldxb st stx
           add sub mul div mod and or xor lsh rsh
           tax txa ret)
       (bvn0l 8)]
      [else (symbv 8)]))

  (define (make-foff name)
    (case name
      [(ld ldi ldh ldb ldx ldxi ldxb st stx
           add sub mul div mod and or xor lsh rsh
           tax txa ret jmp ja)
       (bvn0l 8)]
      [else (symbv 8)]))

  (define instrs
    (for/list ([name op-names])
      (make-instruction
        name
        old-bpf-instr
        (list
          old-bpf-instr-imm
          old-bpf-instr-toff
          old-bpf-instr-foff
          old-bpf-instr-op)
        (list
          (thunk (make-imm name))
          (thunk (make-toff name))
          (thunk (make-foff name))
          (thunk name))
        '(value value value op)
        '(32 8 8 -1)
        (list #t #t #t #f)
        '(imm toff foff op))))

  ; TODO Need to give option for register values to be non-64 bit
  (define regs (make-registers 2 32))
  (define mem (make-memory 16))
  (define-symbolic pc (bitvector 64))
  (make-machine
    (make-state regs mem pc)
    interpret-with-state
    instrs
    (apply old-bpf-instr
           (flatten-list
             (map bvn0l (list 32 8 8))
             'add))
    #f ; Don't need this arg for source machine
    rwset-cache))

; TODO:
;   Ret not synthesized b/c no necessary guarantees
;   Synthesis works despite pc-fact being off (currently 4). Why?
