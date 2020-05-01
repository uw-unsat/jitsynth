#lang rosette

(require "interpreter.rkt")

(require "../common/case-hex.rkt")
(require "../common/instructions.rkt")
(require "../common/data-structures.rkt")
(require (only-in "../common/debug.rkt" [debugln common:debugln]))

(provide decode)
(provide decode-file)

(define DEBUG #f)

(define (debugln x)
  (if DEBUG (common:debugln x) void))

(define (decode-rtype raw)
  (riscv-rtype
    (extract 31 25 raw)
    (extract 24 20 raw)
    (extract 19 15 raw)
    (extract 14 12 raw)
    (extract 11 7 raw)
    (extract 6 0 raw)))

(define (decode-utype raw)
  (riscv-utype
    (extract 31 12 raw)
    (extract 11 7 raw)
    (extract 6 0 raw)))

(define (decode-itype raw)
  (riscv-itype
    (extract 31 20 raw)
    (extract 19 15 raw)
    (extract 11 7 raw)
    (extract 14 12 raw)
    (extract 6 0 raw)))

(define (decode-stype raw)
  (riscv-stype
    (concat (extract 31 25 raw) (extract 11 7 raw))
    (extract 24 20 raw)
    (extract 19 15 raw)
    (extract 14 12 raw)
    (extract 6 0 raw)))

(define (decode-btype raw)
  (riscv-btype
    (concat
      (extract 31 31 raw)
      (extract 7 7 raw)
      (extract 30 25 raw)
      (extract 11 8 raw))
    (extract 24 20 raw)
    (extract 19 15 raw)
    (extract 14 12 raw)
    (extract 6 0 raw)))


(define (decode-jtype raw)
  (riscv-jtype
    (concat
      (extract 31 31 raw)
      (extract 19 12 raw)
      (extract 20 20 raw)
      (extract 30 21 raw))
    (extract 11 7 raw)
    (extract 6 0 raw)))

(define (decode instr-bvs)
  (for/list ([raw instr-bvs])
    (let ([opcode (extract 6 0 raw)])
      (case-hex opcode 7
        ; 32-bit arith
        [(#x3b) (decode-rtype raw)]

        ; 32-bit imm arith
        [(#x1b) (decode-itype raw)]

        ; 64-bit arith
        [(#x33) (decode-rtype raw)]

        ; 64-bit imm arith
        [(#x13) (decode-itype raw)]

        ; loads
        [(#x03) (decode-itype raw)]

        ; stores
        [(#x23) (decode-stype raw)]

        ; uncond jump (jal)
        [(#x6f) (decode-jtype raw)]

        ; cond jumps
        [(#x63) (decode-btype raw)]

        ; lui
        [(#x37) (decode-utype raw)]

        ; jalr
        [(#x67) (decode-itype raw)]

        ; auipc
        [(#x17) (decode-utype raw)]
        [(#x00) (decode-utype raw)]))))

; Assuming RISC-V, 32-bit ops
(define (bytes->bv fin)
  (let ([bval (read-bytes 4 fin)])
    (if (eof-object? bval) #f
      (apply concat
        (reverse
          (map (lambda (num) (bv num 8))
               (bytes->list bval)))))))

(define (bytes-file->bv-list fin)
  (let ([bval (bytes->bv fin)])
    (if (not bval) null
      (cons bval (bytes-file->bv-list fin)))))

(define (line->bv fin)
  (let ([lval (read-line fin)])
    (if (eof-object? lval) #f
      (bv (string->number lval 16) 32))))

(define (text-file->bv-list fin)
  (let ([lval (line->bv fin)])
    (if (not lval) null
      (cons lval (text-file->bv-list fin)))))

(define (decode-file file-name [bytes? #t])
  (let* ([fin (open-input-file file-name)]
         [raw-instrs (if bytes?
                       (bytes-file->bv-list fin)
                       (text-file->bv-list fin))])
    (close-input-port fin)
    (decode raw-instrs)))
