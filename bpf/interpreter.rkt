#lang rosette

(require "../common/data-structures.rkt")
(require "../common/data-utils.rkt")
(require "../common/case-hex.rkt")
(require "../ams/machine.rkt")
(require "../common/instructions.rkt")
(require "../common/symbolic-ops.rkt")
(require (only-in "../common/debug.rkt" [debugln common:debugln]))

(provide interpret-with-state
         interpret-with-init-state
         get-return-value)

(define DEBUG #f)

(define (debugln x)
  (if DEBUG (common:debugln x) void))

(struct op-raw (opcode cls srcbit func dst src offset imm) #:transparent)

(define (le->be vec num-bytes)
  (apply concat (for/list ([i num-bytes]) (extract (+ (* i 8) 7) (* i 8) vec))))

; NOTE: Currently using (inefficient) bitvector->natural conversion here.
(define (apply-bele instr rvals)
  (let* ([opcode (op-raw-opcode instr)]
         [imm (op-raw-imm instr)]
         [dst (op-raw-dst instr)]
         [num-bytes (bitvector->natural (bvlshr imm (bv 3 32)))]
         [orig-rval (extract
                    (- (bitvector->natural imm) 1)
                    0
                    (read-register rvals dst))]
        [value (case-op opcode
                 [(#xdc) (le->be orig-rval num-bytes)]
                 [(#xd4) orig-rval])])
    (write-register rvals dst (zero-extend value (bitvector 64)))))

(define (apply-arith instr rvals)
  (let* ([cls (op-raw-cls instr)]
        [func (op-raw-func instr)]
        [dst (op-raw-dst instr)]
        [dst-rval (read-register rvals dst)]
        [src (op-raw-src instr)]
        [src? (bveq (op-raw-srcbit instr) (bv 1 1))]
        [arg (if src?
               (read-register rvals src)
               (sign-extend (op-raw-imm instr) (bitvector 64)))]
        [alu32? (bveq (bv 4 3) cls)]
        [func (case-hex func 4
                [(#x0) bvadd]
                [(#x1) bvsub]
                [(#x2) (if alu32? smul32 smul)]
                [(#x3) (if alu32? sdiv32 sdiv)]
                [(#x9) (if alu32? smod32 smod)]
                [(#xb) (lambda (a b) b)] ; a is register, b is imm/src
                [(#x8) (lambda (a b) (bvneg a))]
                [(#x5) bvand]
                [(#x4)  bvor]
                [(#xa) bvxor]
                [(#x6)
                 (assert (bvult arg (if alu32? (bv #b100000 64) (bv #b1000000 64))))
                 bvshl]
                [(#x7)
                 (assert (bvult arg (if alu32? (bv #b100000 64) (bv #b1000000 64))))
                 bvlshr]
                ; NOTE: This is the only op that doesn't work by masking
                [(#xc)
                 ; (if alu32?
                 ;   (lambda (a b)
                 ;     (let ([a32 (sign-extend (extract 31 0 a) (bitvector 64))])
                 ;       (bvashr a32 b)))
                 (assert (bvult arg (if alu32? (bv #b100000 64) (bv #b1000000 64))))
                 bvashr])]
        [mask
          (lambda (val)
            (if alu32? (extract 31 0 val) val))]
        [extend
          (lambda (val)
            (if alu32? (zero-extend val (bitvector 64)) val))]
        ; Even 32-bit ops perform 64 bit (mask afterwards)
        [res (extend (apply func (list (mask dst-rval) (mask arg))))])
    (debugln (~a "Arith func: " func))
    (debugln (~a "Arith res: " res))
    (write-register rvals dst res)))

(define (apply-load instr rvals memory)
  (let* ([opcode (op-raw-opcode instr)]
         [offset (sign-extend (op-raw-offset instr) (bitvector 64))]
         [src (op-raw-src instr)]
         [src-val (read-register rvals src)]
         [dst (op-raw-dst instr)]
         [num-bytes (case-op opcode
                      [(#x61) (bv 4 16)]
                      [(#x69) (bv 2 16)]
                      [(#x71) (bv 1 16)]
                      [(#x79) (bv 8 16)])]
         [index (bvadd offset src-val)]
         [memval (zero-extend (read-memory memory index num-bytes) (bitvector 64))])
    (write-register rvals dst memval)))

(define (apply-store instr rvals memory)
  (let* ([opcode (op-raw-opcode instr)]
         [dst (op-raw-dst instr)]
         [dst-val (read-register rvals dst)]
         [src (op-raw-src instr)]
         [offset (sign-extend (op-raw-offset instr) (bitvector 64))]
         [imm (op-raw-imm instr)]
         [num-bytes (case-op opcode
                             [(#x62 #x63) (bv 4 16)]
                             [(#x6a #x6b) (bv 2 16)]
                             [(#x72 #x73) (bv 1 16)]
                             [(#x7a #x7b) (bv 8 16)]
                             )]
         ; NOTE: May need to sign-extend. Semantics unclear.
         [imm? (bveq (op-raw-cls instr) (bv 2 3))]
         [value (if imm? imm (read-register rvals src))]
         [index (bvadd offset dst-val)]
         [new-mem (write-memory memory index num-bytes value)])
    new-mem))


(define (jump-to-pc instr rvals pc)
  (let* ([opcode (op-raw-opcode instr)]
         [cls (op-raw-cls instr)]
         [func (op-raw-func instr)]
         [dst (op-raw-dst instr)]
         [src (op-raw-src instr)]
         [offset (sign-extend (op-raw-offset instr) (bitvector 64))]
         [imm (op-raw-imm instr)]
         [src? (bveq (op-raw-srcbit instr) (bv 1 1))]
         [32bit? (bveq cls (bv 6 3))]
         [left (extract (if 32bit? 31 63) 0 (read-register rvals dst))]
         [right (if src?
                  (extract (if 32bit? 31 63) 0 (read-register rvals src))
                  (if 32bit? imm (sign-extend imm (bitvector 64))))]
         [comparator (case-hex func 4
                       [(#x0) (lambda (a b) #t)] ; ja
                       [(#x2) bvugt]
                       [(#xa) bvult]
                       [(#x3) bvuge]
                       [(#xb) bvule]
                       [(#x6) bvsgt]
                       [(#xc) bvslt]
                       [(#x7) bvsge]
                       [(#xd) bvsle]
                       [(#x4) (lambda (a b)
                                (not (bveq (bvand a b)
                                           ((if 32bit? bv32 bv64) 0))))] ; jset
                       [(#x5) (lambda (a b)
                                (not (bveq a b)))] ; jne
                       [(#x1) bveq])]
        [jmp-offset (bvadd offset (bv 1 64))])
    (if (apply comparator (list left right))
      (bvadd jmp-offset pc)
      (bvadd pc (bv 1 64)))))

(define (interpret-with-init-state
          instructions [max-depth (bv -1 64)]
          #:allow-bele? [allow-bele? #f])
  (interpret-with-state
    instructions
    (make-zerod-registers 10)
    (make-zerod-memory 100)
    (bv 0 64)
    max-depth
    #:allow-bele? allow-bele?))

(define (interpret-with-state
          instructions registers memory pc
          [max-depth (bv -1 64)] #:allow-bele? [allow-bele? #f])
  (interpret
    instructions
    (state
      registers
      memory
      pc
      #f)
    pc
    (bv 0 64)
    max-depth
    #:allow-bele? allow-bele?))

(define (interpret instructions curr-state base-pc depth max-depth #:allow-bele? [allow-bele? #f])
  (debugln (~a "PC: " (state-pc curr-state)))
  (if (or (bvuge depth max-depth)
          (state-return? curr-state)
          (empty? instructions)
          (ge-instruction-length? instructions (state-pc curr-state) base-pc (bv 1 64)))
    curr-state
      (let* ([rvals (state-rvals curr-state)]
             [memory (state-memory curr-state)]
             [pc (state-pc curr-state)]
             [new-pc (bvadd pc (bv 1 64))]
             [bi (read-instruction instructions pc base-pc (bv 1 64))]
             [should-return #f]
             [cls (bpf-instr-cls bi)]
             [srcbit (bpf-instr-srcbit bi)]
             [func (bpf-instr-func bi)]
             [opcode (bpf-instr-op bi)]
             [dst (bpf-instr-dst bi)]
             [src (bpf-instr-src bi)]
             [offset (bpf-instr-off bi)]
             [imm (bpf-instr-imm bi)]
             [instr (op-raw opcode cls srcbit func dst src offset imm)])
        (begin
          (debugln (~a "OP: " opcode))
          (case-op opcode
            ; lddw
            [(#x18) (set! rvals (write-register rvals dst (sign-extend imm (bitvector 64))))]
            
            ; ldxw, ldxh, ldxb, ldxdw
            [(#x61 #x69 #x71 #x79) (set! rvals (apply-load instr rvals memory))]
            
            ; "stw" "sth" "stb" "stdw" "stxw" "stxh" "stxb" "stxdw"
            [(#x62 #x6a #x72 #x7a #x63 #x6b #x73 #x7b) (set! memory (apply-store instr rvals memory))]
            
            ; "ja" "jeq" "jgt" "jlt" "jge" "jle" "jsgt" "jslt" "jsge" "jsle" "jset" "jne"
            [(#x05 #x15 #x1d #x25 #x2d #x35 #x3d #xa5 #xad #xb5 #xbd
                   #x45 #x4d #x55 #x5d #x65 #x6d #x75 #x7d #xc5 #xcd #xd5 #xdd
                   #x06 #x16 #x1e #x26 #x2e #x36 #x3e #xa6 #xae #xb6 #xbe
                   #x46 #x4e #x56 #x5e #x66 #x6e #x76 #x7e #xc6 #xce #xd6 #xde) (set! new-pc (jump-to-pc instr rvals pc))]

            ; "add" "sub" "mul" "div" "mod" "mov" "neg" "and" "or" "xor" "lsh" "rsh" "arsh"
            [(#x07 #x0f #x17 #x1f #x27 #x2f #x37 #x3f #x47 #x4f #x57 #x5f
                   #x67 #x6f #x77 #x7f #x87 #x97 #x9f #xa7 #xaf #xb7 #xbf #xc7 #xcf
            ; "add32" "sub32" "mul32" "div32" "or32" "and32" "lsh32" "rsh32"
            ; "mod32" "xor32" "mov32" "arsh32" "neg32"
                    #x04 #x0c #x14 #x1c #x24 #x2c #x34 #x3c #x44 #x4c #x54 #x5c #x64
                    #x6c #x74 #x7c #x84 #x94 #x9c #xa4 #xac #xb4 #xbc #xc4 #xcc)
             (set! rvals (apply-arith instr rvals))]

            ; "be16" "be32" "be64" "le16" "le32" "le64"
            ; NOTE: Commented out due to strange semantics (introduces integers).
            ;       Can support in the future if needed.
            [(#xd4 #xdc)
             (if allow-bele?
               (set! rvals (apply-bele instr rvals))
               (void))]

            ; exit
            [(#x95) (set! should-return #t)]

            ; "call"
            ; NOTE: Currently not supported.
            [(#x85) #f])
          (interpret instructions (state rvals memory new-pc should-return)
                     base-pc (bvadd depth (bv 1 64))
                     max-depth #:allow-bele? allow-bele?)))))

(define (get-return-value st)
  (read-register (state-regs st) (bv 0 4)))
