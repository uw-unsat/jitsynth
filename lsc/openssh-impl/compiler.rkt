#lang rosette

; (struct old-bpf-instr (imm toff foff op) #:transparent)
(require "../../common/instructions.rkt"
         "../../common/data-utils.rkt")
(require file/glob)

(define ALLOW (bv32 0))
(define DENY (bv32 1))
(define KILL (bv32 2))

(struct sc-instr (name nr arg_nr arg_val) #:transparent)

(define (seccomp-compile instructions)
  (cond
    [(empty? instructions) null]
    [else
      ; TODO something for default
      (append
        (let* ([instr (first instructions)]
               [nr (sc-instr-nr instr)]
               [arg_nr (sc-instr-arg_nr instr)]
               [arg_val (sc-instr-arg_val instr)])
          (case (sc-instr-name instr)
            [(SC_KILL)
             (list
               (old-bpf-instr nr (bv8 0) (bv8 1) 'jeq)
               (old-bpf-instr KILL (bv8 0) (bv8 0) 'ret))]
            [(SC_DENY)
             (list
               (old-bpf-instr nr (bv8 0) (bv8 1) 'jeq)
               (old-bpf-instr DENY (bv8 0) (bv8 0) 'ret))]
            [(SC_ALLOW)
             (list
               (old-bpf-instr nr (bv8 0) (bv8 1) 'jeq)
               (old-bpf-instr ALLOW (bv8 0) (bv8 0) 'ret))]
            [(SC_ALLOW_ARG)
             (list 
               (old-bpf-instr nr (bv8 0) (bv8 1) 'jeq)
               (old-bpf-instr arg_nr (bv8 0) (bv8 0) 'ldi)
               (old-bpf-instr (bvand (bv32 #xFFFFFFFF) arg_val) (bv8 0) (bv8 3) 'jeq)
               (old-bpf-instr (bvadd (bv32 32) arg_nr) (bv8 0) (bv8 0) 'ldi)
               (old-bpf-instr (bvand (bv32 #xFFFFFFFF) (bvlshr arg_val (bv32 32))) (bv8 0) (bv8 3) 'jeq)
               (old-bpf-instr ALLOW (bv8 0) (bv8 0) 'ret)
               (old-bpf-instr (bv32 0) (bv8 0) (bv8 0) 'ldi))]
            [(DEFAULT)
             (list (old-bpf-instr nr (bv8 0) (bv8 0) 'ret))]))
        (seccomp-compile (rest instructions)))]))

(define (parse-line line)
  (define args
    (map
      string->symbol
      (string-split line)))
  (case (first args)
    [(ARG) (sc-instr 'SC_ALLOW_ARG (bv32 0) (bv32 0) (bv32 0))]
    [(RULE) (sc-instr 'SC_DENY (bv32 0) (bv32 0) (bv32 0))]
    [(DEFAULT) (sc-instr 'DEFAULT (bv32 0) (bv32 0) (bv32 0))]))

(define (parse-op opcode)
  (let ([op opcode])
    (bv16
      ; TODO these are made-up
      (case op
        [(add) 0]
        [(sub) 1]
        [(mul) 2]
        [(div) 3]
        [(mod) 4]
        [(and) 5]
        [(or) 6]
        [(xor) 7]
        [(lsh) 8]
        [(rsh) 9]
        [(tax) 10]
        [(txa) 11]
        ; TODO implement manually?
        [(ret) 12]
        [(ld) 13]
        [(ldi) 14]
        [(ldh) 15]
        [(ldb) 16]
        [(ldx) 17]
        [(ldxi) 18]
        [(ldxb) 19]
        [(st) 20]
        [(stx) 21]
        [(jmp) 22]
        [(ja) 23]
        [(jeq) 24]
        [(jneq) 25]
        [(jne) 26]
        [(jlt) 27]
        [(jle) 28]
        [(jgt) 29]
        [(jge) 30]
        [(jset) 31]))))

; (struct old-bpf-instr (imm toff foff op) #:transparent)
(define (obpf->string oi)
  (~a #:min-width 16 #:align 'right #:left-pad-string "0"
      (number->string
        (bitvector->natural
          (concat (old-bpf-instr-imm oi)
                  (old-bpf-instr-toff oi)
                  (old-bpf-instr-foff oi)
                  (parse-op (old-bpf-instr-op oi))))
        16)))

(define dir "lsc/benchmarks")
(define file-names (glob (~a dir "/*.lsc")))
(for ([fn file-names])
  (displayln (~a "At file " fn))
  (define fin (open-input-file fn))
  (define fout (open-output-file (~a fn ".openssh.obpf") #:exists 'replace))
  (define obpf-prog
    (seccomp-compile
      (let f ()
        (let ([line (read-line fin)])
          (if (eof-object? line)
            null
            (cons (parse-line line) (f)))))))
  (displayln (length obpf-prog) fout)
  (for ([instr obpf-prog])
    (displayln (obpf->string instr) fout))
  (close-output-port fout)
  (close-input-port fin))
