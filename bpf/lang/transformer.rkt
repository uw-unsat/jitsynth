#lang rosette

(require "op.rkt")
(require "../../common/instructions.rkt")
(require syntax/strip-context)
(provide string->bv)
(provide transform)
(provide transform-raw)
(provide transform-bpf-instr)

(define (string->bv str [base 10] [bitlen 64])
  (integer->bitvector
    (let ([num (string->number str base)])
      (if (integer? num)
        num
        (error "Number not an integer")))
    (bitvector bitlen)))

(define reg-format #px"r\\d")
(define num-format #px"(0x[\\dabcdef]+)|(-?\\d+)")
(define hex-num-format #px"0x[\\dabcdef]+")
(define mem-format #px"\\[r\\d\\+\\d+\\]")
; NOTE: + will get taken out in initial read
(define jmp-offset-format #px"[\\+-]?\\d+")

; Abstract expressions allowed in rewrite rules
(define abs-reg-format #px"R[a-z]*")
(define abs-const-format #px"C[a-z]*")
(define abs-expr-format #px"\\([^\\)]*\\)")
(define abs-instr-format #px"I[a-z]*")

; NOTE: This is the only part of an op actually stored as a bitvector.
;       Also, I'm not handling the parens at all here.
; Assume (-|0x|)N format
(define (read-number str abs?)
  (if (and abs? (regexp-match-exact? abs-const-format str))
      (string->symbol str)
      (if (regexp-match-exact? hex-num-format str)
          (string->bv (substring str 2) 16 32)
          (string->bv str 10 32))))

; Assume rN format
(define (read-register str abs?)
  (if (and abs? (regexp-match-exact? abs-reg-format str)) (string->symbol str)
      (string->number (substring str 1))))

; NOTE: not allowing "-offset"
; Assume [rN+M] format
(define (read-memory str abs?)
  (let* ([args (string-split str #px"[\\[\\]]|(?=[\\+-])")]
         [first-arg (first args)]
         [second-arg (if (> (length args) 1) (first (rest args)) "0")])
    (memory (read-register first-arg abs?) (read-offset second-arg abs?))))

; Assume (+|-)N format
(define (read-offset str abs?)
  (if (regexp-match-exact? jmp-offset-format str) (string->number str)
      ; check for C[a-z]*
      (if (and abs? (regexp-match-exact? abs-const-format str)) (string->symbol str)
          (error "Invalid jump offset format"))))

; NOTE: May need to handle abstract parameters when considering rewrite rules
(define (update-imm-or-src opval param abs?)
  (if (regexp-match-exact? reg-format param) (op (op-name opval) (op-dst opval) (read-register param abs?) (op-offset opval) (op-imm opval))
      (if (regexp-match-exact? num-format param) (op (op-name opval) (op-dst opval) (op-src opval) (op-offset opval) (read-number param abs?))
          (error "No valid immediate or source value"))))

(define (make-op cmd params abs?)
  (case cmd
    [("neg" "neg32") (op cmd (read-register (list-ref params 0) abs?) #f #f (bv 0 32))]
    [("le16" "le32" "le64" "be16" "be32" "be64")
     (let ([imm (string->number (substring cmd 2))])
       (op cmd (read-register (list-ref params 0) abs?) #f #f (bv imm 32)))]
    [("add" "sub" "mul" "div" "or" "and" "lsh" "rsh" "mod" "xor" "mov" "arsh"
            "add32" "sub32" "mul32" "div32" "or32" "and32" "lsh32" "rsh32" "mod32" "xor32" "mov32" "arsh32")
     (let ([opval (op cmd (read-register (list-ref params 0) abs?) #f #f #f)])
       (update-imm-or-src opval (list-ref params 1) abs?))]
    [("lddw") (op cmd (read-register (list-ref params 0) abs?) #f #f (read-number (list-ref params 1) abs?))]
    [("ldxw" "ldxh" "ldxb" "ldxdw") (let ([regval (read-register (list-ref params 0) abs?)]
                  [memval (read-memory (list-ref params 1) abs?)])
              (op cmd regval (memory-register memval) (memory-offset memval) #f))]
    [("stw" "sth" "stb" "stdw") (let ([memval (read-memory (list-ref params 0) abs?)]
                  [immval (read-number (list-ref params 1) abs?)])
              (op cmd (memory-register memval) #f (memory-offset memval) immval))]
    [("stxw" "stxh" "stxb" "stxdw") (let ([memval (read-memory (list-ref params 0) abs?)]
                   [regval (read-register (list-ref params 1) abs?)])
               (op cmd (memory-register memval) regval (memory-offset memval) #f))]
    [("ja") (op cmd #f #f (read-offset (list-ref params 0) abs?) #f)]
    [("jeq" "jgt" "jlt" "jge" "jle" "jsgt" "jslt" "jsge" "jsle" "jset" "jne")
     (let ([opval (op cmd (read-register (list-ref params 0) abs?) #f (read-offset (list-ref params 2) abs?) #f)])
       (update-imm-or-src opval (list-ref params 1) abs?))]
    [("call") (op cmd #f #f #f (read-number (list-ref params 0) abs?))]
    [("exit") (op cmd #f #f #f #f)]
    ; NOTE: Currently not supporting abstract instructions at this level
    ;       (possible in rewrite rules)
    [else (error "Unrecognized operation")]))

; NOTE: Only handles symbols and numbers
(define (symtok->string symtok)
  (if (symbol? symtok) (symbol->string symtok)
      (if (number? symtok) (number->string symtok)
          (error "Unrecognized token"))))

(define (filter line abs?)
  (let* ([strtoks (map symtok->string line)]
         [cmd (first strtoks)]
         [params (rest strtoks)])
    (case cmd
      [("exit") (= (length params) 0)]
      [("neg" "neg32" "le16" "le32" "le64" "be16" "be32" "be64" "ja" "call") (= (length params) 1)]
      [("lddw" "ldxw" "ldxh" "ldxb" "ldxdw"
               "stw" "sth" "stb" "stdw" "stxw" "stxh" "stxb" "stxdw"
               "add" "sub" "mul" "div" "or" "and" "lsh" "rsh" "mod" "xor" "mov" "arsh"
               "add32" "sub32" "mul32" "div32" "or32" "and32" "lsh32" "rsh32" "mod32" "xor32" "mov32" "arsh32")
       (= (length params) 2)]
      [("jeq" "jgt" "jge" "jlt" "jle" "jsgt" "jslt" "jsge" "jsle" "jset" "jne") (= (length params) 3)]
      [else (and abs? (regexp-match-exact? abs-instr-format cmd) (= (length params) 4))])))

(define (transform lines [abs? #f])
  (if (empty? lines) null
      (let* ([curr-line (first lines)]
             [remaining-lines (rest lines)])
        (if (or (void? curr-line) (empty? curr-line)) (transform remaining-lines)
            (if (not (filter curr-line abs?)) (error "Malformed line")
                (let* ([strtoks (map symtok->string curr-line)]
                       [line-cmd (first strtoks)]
                       [line-params (rest strtoks)])
                  (cons (make-op line-cmd line-params abs?) (transform remaining-lines abs?))))))))

(define (transform-raw lines [abs? #f])
  (map op->raw (transform lines abs)))

(define (transform-bpf-instr lines [abs? #f])
  (map raw->bpf-instr (transform-raw lines abs)))
