#lang rosette

(require "../ams/machine.rkt" "../ams/machine-loader.rkt"
         "../common/instructions.rkt" "../common/data-utils.rkt"
         "../common/data-structures.rkt" "interpreter.rkt")

(require rosette/lib/angelic)

(provide make-bpf-machine)

(define (all-bpf-names #:j32 [jmp32? #f])
  (append
    '(addi add subi sub muli mul divi div ori or andi and lshi lsh
           rshi rsh neg modi mod xori xor movi mov arshi arsh
           lddw ldxw ldxh ldxb ldxdw
           stw sth stb stdw stxw stxh stxb stxdw
           ja jeqi jeq jgti jgt jgei jge jlti jlt jlei jle jseti jset jnei jne
           jsgti jsgt jsgei jsge jslti jslt jslei jsle
           addi32 add32
           subi32 sub32 muli32 mul32 divi32 div32 ori32 or32 andi32 and32 lshi32 lsh32
           rshi32 rsh32 neg32 modi32 mod32 xori32 xor32 movi32 mov32 arshi32 arsh32
           exit)
    (if jmp32?
      '(jlei32 jle32 jseti32 jset32 jnei32 jne32
               ja32 jeqi32 jeq32 jgti32 jgt32 jgei32 jge32 jlti32 jlt32
               jsgti32 jsgt32 jsgei32 jsge32 jslti32 jslt32 jslei32 jsle32)
      null)))

; All BPF loads, stores, jumps, and arith
(define (all-bpf-opcodes #:j32 [jmp32? #f])
  (map (lambda (x)
         (let ([op (bv x 8)])
           (list (extract 7 4 op) (extract 3 3 op) (extract 2 0 op))))
       (append
         ; 64-bit arith
         (list #x07 #x0f #x17 #x1f #x27 #x2f #x37 #x3f #x47 #x4f #x57 #x5f
               #x67 #x6f #x77 #x7f #x87 #x97 #x9f #xa7 #xaf #xb7 #xbf #xc7 #xcf
               ; Loads/stores
               #x18 #x61 #x69 #x71 #x79 #x62 #x6a #x72 #x7a #x63 #x6b
               #x73 #x7b
               ; Jumps
               #x05 #x15 #x1d #x25 #x2d #x35 #x3d #xa5 #xad #xb5
               #xbd #x45 #x4d #x55 #x5d #x65 #x6d #x75 #x7d #xc5 #xcd #xd5 #xdd
               ; 32-bit Arith
               #x04 #x0c #x14 #x1c #x24 #x2c #x34 #x3c #x44 #x4c #x54 #x5c #x64
               #x6c #x74 #x7c #x84 #x94 #x9c #xa4 #xac #xb4 #xbc #xc4 #xcc
               ; exit
               #x95)
         (if jmp32?
           ; 32-bit Jumps
           (list #x06 #x16 #x1e #x26 #x2e #x36 #x3e #xa6 #xae #xb6 #xbe
                 #x46 #x4e #x56 #x5e #x66 #x6e #x76 #x7e #xc6 #xce #xd6 #xde)
           null))))

(define (name->code name #:j32 [jmp32? #f])
  (let ([all-names (all-bpf-names #:j32 jmp32?)]
        [all-codes (all-bpf-opcodes #:j32 jmp32?)])
    (list-ref all-codes (index-of all-names name))))

; BPF machine definition
(define (make-bpf-machine
          [bpf-names (all-bpf-names)]
          #:rwset-cache [rwset-cache "bpf/rwset-cache"]
          #:lddw32 [lddw32? #f])
  (define bpf-opcodes (map name->code bpf-names))

  (define (bpf-symreg)
    (define regind
      (apply choose* (build-list 11 (bvl 4))))
    regind)

  (define (is-jump? bpf-op)
    (and (or (bveq (third bpf-op) (bv 5 3))
             (bveq (third bpf-op) (bv 6 3)))
         (not (bveq (first bpf-op) (bv 9 4)))))

  (define (is-store? bpf-op)
    (or (bveq (third bpf-op) (bv 2 3))
        (bveq (third bpf-op) (bv 3 3))))

  (define (is-load? bpf-op)
    (bveq (third bpf-op) (bv 1 3)))

  (define (is-arith? bpf-op)
    ; 32-bit
    (or (bveq (third bpf-op) (bv 4 3))
        ; 64-bit
        (bveq (third bpf-op) (bv 7 3))))

  (define (name->opcode bpf-name)
    (list-ref bpf-opcodes (index-of bpf-names bpf-name)))

  (define (make-src-reg bpf-name)
    (let ([opcode (name->opcode bpf-name)])
      ; Store of src
      (if (or (bveq (third opcode) (bv 3 3))
              ; Load
              (is-load? opcode)
              ; Src bit is true
              (and (bveq (second opcode) (bv 1 1))
                   (or (is-arith? opcode)
                       (is-jump? opcode))))
        (bpf-symreg)
        (bvn0l 4))))

  (define (make-dst-reg bpf-name)
    (if (and (not (equal? bpf-name 'ja))
             (not (equal? bpf-name 'exit)))
      (bpf-symreg)
      (bvn0l 4)))

  (define (make-imm bpf-name)
    (let ([opcode (name->opcode bpf-name)])
      (if (equal? bpf-name 'lddw)
        (symbv (if lddw32? 32 64))
        ; Src bit is false
        (if (or (and (bveq (second opcode) (bv 0 1))
                     (or (is-arith? opcode)
                         (is-jump? opcode)))
                ; Store of imm
                (bveq (third opcode) (bv 2 3)))
          (symbv 32)
          (bvn0l 32)))))

  (define (make-off bpf-name)
    (let ([opcode (name->opcode bpf-name)])
      (if (or (is-jump? opcode)
              (is-load? opcode)
              (is-store? opcode))
        (symbv 16)
        (bvn0l 16))))

  (define (apply-instr-assumptions bi)
    (assert (bvult (bpf-instr-off bi) (bv #x8000 16))))

  (define (params->bpf-instr imm off src dst op-list)
    (let ([bi (apply bpf-instr (flatten-list imm off src dst op-list))])
      (apply-instr-assumptions bi)
      bi))

  (define (name->bpf-instr bpf-name) 
    (let ([opcode (name->opcode bpf-name)]
          [dst (make-dst-reg bpf-name)]
          [src (make-src-reg bpf-name)]
          [off (make-off bpf-name)]
          [imm (make-imm bpf-name)])
      (apply bpf-instr (flatten-list imm off src dst opcode))))

  (define params-list (list 'imm 'off 'src 'dst 'op))

  (define bpf-instructions
    (for/list ([name bpf-names])
      (make-instruction
        name
        params->bpf-instr
        (list
          bpf-instr-imm
          bpf-instr-off
          bpf-instr-src
          bpf-instr-dst
          bpf-instr-op)
        (list
          (thunk (make-imm name))
          (thunk (make-off name))
          (thunk (make-src-reg name))
          (thunk (make-dst-reg name))
          (thunk (name->opcode name)))
        (list 'value 'value 'reg 'reg 'op)
        (list (if (equal? name 'lddw) 64 32) 16 4 4 -1)
        (list #t #t #t #t #f)
        ;      (also, can generate it automatically based on (symbolic?))
        (list 'imm 'off 'src 'dst 'op))))

  (define bpf-regs (make-registers 11 64))
  (define bpf-mem (make-memory 128))
  (define-symbolic bpf-pc (bitvector 64))
  (define-values (bpf-machine); bpf-machine-asserts)
    (make-machine
      (make-state bpf-regs bpf-mem bpf-pc)
      interpret-with-state
      bpf-instructions
      (apply bpf-instr
             (flatten-list
               (map bvn0l (list 32 16 4 4 4 1))
               (bv 7 3)))
      (thunk* ; Function that returns parameter getters
        (list bpf-instr-imm
              bpf-instr-off
              bpf-instr-src
              bpf-instr-dst
              bpf-instr-func
              bpf-instr-srcbit
              bpf-instr-cls))
      rwset-cache))

  bpf-machine)
