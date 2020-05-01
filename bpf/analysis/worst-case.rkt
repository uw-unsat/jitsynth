#lang rosette

(require "../common/data-structures.rkt")
(require "../common/data-utils.rkt")
(require "../common/case-hex.rkt")
(require "../ams/machine.rkt")
(require "../common/instructions.rkt")
(require "../common/symbolic-ops.rkt")
(require (only-in "../common/debug.rkt" [debugln common:debugln]))

(require file/glob)

(define DEBUG #f)

(define (debugln x)
  (if DEBUG (common:debugln x) void))

(define (bpf-nop? instr)
  (and (bveq (bpf-instr-imm instr) (bv32 0))
       (bveq (bpf-instr-off instr) (bv16 0))
       (bveq (bpf-instr-src instr) (bv 0 4))
       (bveq (bpf-instr-dst instr) (bv 0 4))
       (bveq (bpf-instr-func instr) (bv 0 4))
       (bveq (bpf-instr-srcbit instr) (bv 0 1))
       (bveq (bpf-instr-cls instr) (bv 7 3))))

(define prog->res (hash))
(define-syntax-rule (set-and-return prog res)
  (if (hash-has-key? prog->res prog)
    (hash-ref prog->res prog)
    (let ([comp-res res])
      (set! prog->res (hash-set prog->res prog comp-res))
      comp-res)))

; Do I actually need this function? Or is this easier to do at kernel level?
; NOTE: Assuming no backwards jumps
(define (worst-case-exec instructions)
  (set-and-return
    instructions
    (if (empty? instructions)
      0
      (let* ([instr (first instructions)]
             [opcode (bpf-instr-op instr)]
             [offset (add1 (bitvector->integer (bpf-instr-off instr)))])
        (begin
          (debugln (~a "OP: " opcode))
          (case-op opcode
            ; lddw
            ; ldxw, ldxh, ldxb, ldxdw
            ; "stw" "sth" "stb" "stdw" "stxw" "stxh" "stxb" "stxdw"
            ; "add" "sub" "mul" "div" "mod" "mov" "neg" "and" "or" "xor" "lsh" "rsh" "arsh"
            ; "add32" "sub32" "mul32" "div32" "or32" "and32" "lsh32" "rsh32"
            ; "mod32" "xor32" "mov32" "arsh32" "neg32"
            ; "call"
            [(#x18
              #x61 #x69 #x71 #x79
              #x62 #x6a #x72 #x7a #x63 #x6b #x73 #x7b
              #x07 #x0f #x17 #x1f #x27 #x2f #x37 #x3f #x47 #x4f #x57 #x5f
              #x67 #x6f #x77 #x7f #x87 #x97 #x9f #xa7 #xaf #xb7 #xbf #xc7 #xcf
              #x04 #x0c #x14 #x1c #x24 #x2c #x34 #x3c #x44 #x4c #x54 #x5c #x64
              #x6c #x74 #x7c #x84 #x94 #x9c #xa4 #xac #xb4 #xbc #xc4 #xcc
              #x85)
             (+ (worst-case-exec (rest instructions))
                ; NOTE: This ignores nops in worst-case execution time
                (if (bpf-nop? instr) 0 1))]
            ; "ja"
            [(#x05)
             (add1 (worst-case-exec (list-tail instructions offset)))]

            ; "jeq" "jgt" "jlt" "jge" "jle" "jsgt" "jslt" "jsge" "jsle" "jset" "jne"
            [(#x15 #x1d #x25 #x2d #x35 #x3d #xa5 #xad #xb5 #xbd
              #x45 #x4d #x55 #x5d #x65 #x6d #x75 #x7d #xc5 #xcd #xd5 #xdd
              #x06 #x16 #x1e #x26 #x2e #x36 #x3e #xa6 #xae #xb6 #xbe
              #x46 #x4e #x56 #x5e #x66 #x6e #x76 #x7e #xc6 #xce #xd6 #xde)
             (add1
               (max
                 (worst-case-exec (rest instructions))
                 (worst-case-exec (list-tail instructions offset))))]

            ; exit
            [(#x95) 1]))))))

(define (parse-line line)
  (raw->bpf-instr (bv64 (string->number line 16))))

(define (parse-bytes bts)
  ; (displayln (apply concat (reverse (map bv8 (bytes->list bts)))))
  (raw->bpf-instr (apply concat (reverse (map bv8 (bytes->list bts))))))

; Code for counting program worst-case exec of jitsynth/linux old bpf->ebpf
; (define dir "linux/benchmarks/jitk")
; (define dir "lsc2bpf/scossh-ebpf-compiled")
(define dir "lsc/benchmarks/ebpf")
(define js-file-names (glob (~a dir "/*.ebpf.bin.out")))
; (define linux-file-names (glob (~a dir "/*.ebpf.bin")))
; (displayln "benchmark,jitsynth,linux")
; (displayln "benchmark,jitsynth-ebpf")
(displayln "benchmark,linux-ebpf")
(for ([js-fn js-file-names])
      ; [linux-fn linux-file-names])
  (define benchmark (last (string-split (path->string js-fn) #px"(/|\\.|(ebpf\\.bin\\.out))+")))
  ; (displayln (~a "At file " fn))
  (define js-fin (open-input-file js-fn))
  ; (define linux-fin (open-input-file linux-fn))
  (define js-prog
    (let f ()
      (let ([line (read-line js-fin)])
        (if (eof-object? line)
          null
          (cons (parse-line line) (f))))))
  #|
  (define linux-prog
    (let f ()
      (let ([bts (read-bytes 8 linux-fin)])
        (if (eof-object? bts)
          null
          (cons (parse-bytes bts) (f))))))
  |#
  (displayln (~a benchmark "," (worst-case-exec js-prog)))); "," (worst-case-exec linux-prog))))
