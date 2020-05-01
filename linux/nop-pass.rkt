#lang rosette

(require "../riscv/decoder.rkt")
(require "../common/case-hex.rkt")
(require "../common/instructions.rkt")
(require "../common/data-structures.rkt")
(require (only-in "../common/debug.rkt" [debugln common:debugln]))

(require file/glob)

(provide remove-nops)

(define DEBUG #t)

(define (debugln x)
  (if DEBUG (common:debugln x) (void)))

(define (remove-nops instrs)
  ; We'll say that the range is inclusive start, exclusive end
  (define (nops-in-range start end)
    (debugln (~a "Range: [" start ", " end ")"))
    (let ([high-pcs (memf (lambda (pc) (< pc end)) nop-pcs)]
          [low-pcs (memf (lambda (pc) (< pc start)) nop-pcs)])
      (if high-pcs (- (length high-pcs) (length low-pcs)) 0)))

  (define pc->new-ops (hash))

  (define nop-pcs (list))

  ; TODO include the x4?
  ; Compute nop/zero locations
  (for ([i (range 0 (length instrs))])
    (if (or (riscv-nop? (list-ref instrs i))
            (riscv-zero? (list-ref instrs i)))
      (set! nop-pcs (cons (* 4 i) nop-pcs))
      (void)))
  (debugln (~a "Computed nop PCs: " nop-pcs))

  ; Compute new offsets
  (for ([i (range 0 (length instrs) 9)]
        #:break (or (riscv-zero? (list-ref instrs i))
                    (<= (length instrs) (+ i 8))))
    ; TODO NEXT can no longer assume that the uoff and ioff are at these locations!!
    ; TODO remove this let
    (for ([k (range i (+ 9 i))])
      (debugln (~a "Instr[" k "] = " (list-ref instrs k))))
    (for ([j (range (+ 0 i) (+ 9 i))])
      (let ([insn (list-ref instrs j)])
        (if (riscv-btype? insn)
          (let* ([old-off (bitvector->integer (riscv-instr-imm insn))]
                 [old-pc (* 4 j)]
                 [new-pc (+ old-pc old-off)]
                 [start (min old-pc new-pc)]
                 [end (max old-pc new-pc)]
                 [num-nops (nops-in-range start end)]
                 [new-off
                   (if (< old-pc new-pc)
                     (- old-off (* 4 num-nops))
                     (+ old-off (* 4 num-nops)))]
                 [new-off-bv (integer->bitvector (quotient new-off 2) (bitvector 12))]
                 [branch-instr
                   (riscv-btype
                     new-off-bv
                     (riscv-btype-rs2 insn)
                       (riscv-btype-rs1 insn)
                       (riscv-btype-func3 insn)
                       (riscv-btype-op insn))])
            (debugln "At Branch")
            (debugln (~a "Nops: " num-nops))
            (debugln (~a "Old off: " old-off ", New off: " new-off))
            (debugln (~a "BRANCH: " branch-instr))
            ; TODO name this bettter? eg index->...
            (set! pc->new-ops
              (hash-set pc->new-ops j branch-instr)))
          (if (and (bveq (bv #x67 7) (riscv-instr-op insn))
                   (not (riscv-nop? (list-ref instrs i))))
            ; NOTE: For now, assuming offset is small
            ; TODO NEXt I don't thtink this assumption holds, need to make sure correct value gets loaded
            (let* ([off-reg (riscv-instr-src1 insn)]
                   ; NOTE: Calculating this is super jank,
                   ;       Assumes offset load happens second
                   [lui-off-loc
                     (last
                       (filter
                         (lambda (insn-index)
                           (let ([lui-insn (list-ref instrs insn-index)]
                                 [addi-insn (list-ref instrs (add1 insn-index))])
                             (and (riscv-utype? lui-insn)
                                  (riscv-itype? addi-insn))))
                         (range i (+ i 4))))]
                   [addi-off-loc (add1 lui-off-loc)]
                   [uoff (list-ref instrs lui-off-loc)]
                   [ioff (list-ref instrs addi-off-loc)]
                   [old-off
                     (bitvector->integer
                       (sign-extend
                         (bvadd
                           (riscv-instr-imm uoff)
                           (sign-extend (riscv-instr-imm ioff) (bitvector 32)))
                         (bitvector 64)))]
                   [old-pc (* 4 i)] ; NOTE: Start from i b/c this should be the auipc
                   [new-pc (+ old-pc old-off)]
                   [start (min old-pc new-pc)]
                   [end (max old-pc new-pc)]
                   [num-nops (nops-in-range start end)]
                   [new-off
                     (if (< old-pc new-pc)
                       (- old-off (* 4 num-nops))
                       (+ old-off (* 4 num-nops)))]
                   [new-off-bv (integer->bitvector new-off (bitvector 32))]
                   [first20 (extract 31 12 new-off-bv)]
                   [last12 (extract 11 0 new-off-bv)]
                   [lui-imm
                     (bvadd first20 (zero-extend (extract 11 11 new-off-bv) (bitvector 20)))]
                   [addi-imm last12]
                   [lui-instr
                     (riscv-utype
                       lui-imm
                       (riscv-utype-rd uoff)
                       (riscv-utype-op uoff))]
                   [addi-instr
                     (riscv-itype
                       addi-imm
                       (riscv-itype-rs1 ioff)
                       (riscv-itype-rd ioff)
                       (riscv-itype-func3 ioff)
                       (riscv-itype-op ioff))])
              (debugln "At JALR")
              (debugln (~a "Nops: " num-nops))
              (debugln (~a "Old off: " old-off ", New off: " new-off))
              (debugln (~a "LUI: " lui-instr))
              (debugln (~a "ADDI: " addi-instr))
              (set! pc->new-ops
                (hash-set pc->new-ops lui-off-loc lui-instr))
              (set! pc->new-ops
                (hash-set pc->new-ops addi-off-loc addi-instr)))
              #f)))))
  (debugln "Computed offsets")

  ; Replace jumps/offset loads
  (set! instrs
    (for/list ([i (length instrs)])
      (let ([insn (list-ref instrs i)])
        (hash-ref pc->new-ops i insn))))

  ; Remove nops & zeros
  (set! instrs
    (filter
      (lambda (i)
        (and (not (riscv-nop? i))
             (not (riscv-zero? i))))
      instrs))

  (debugln "Done removing nops")
  instrs)


#| ############# Testing & Debuging ############### |#
#|
(define fn (~a dir "/qemu.rv.bin"))
(define fin (open-input-file fn))
(define raw-instrs
  (for/list ([line (in-lines fin)])
    (bv (string->number line 16) 32)))
(close-input-port fin)
; (define fout (open-output-file fn #:exists 'replace))
(define instrs (decode raw-instrs))
(displayln (length instrs))
(define instrs-w/o-nops (remove-nops instrs))
(displayln (length instrs-w/o-nops))
(define instrs-w/o-nops2 (remove-nops instrs-w/o-nops))
(displayln (length instrs-w/o-nops2))
|#

(define (bv->bytes bitvec)
  (apply bytes
    (for/list ([i (range 0 32 8)])
      (bitvector->natural (extract (+ i 7) i bitvec)))))

(define dir "linux/compiled/jitk-synth-kaby")
(define file-names (glob (~a dir "/*.rv.bin")))
; (define file-names (list (~a dir "/tor.rv.bin")))
(for ([fn file-names])
  (debugln (~a "At file " fn))
  (define instrs (decode-file fn))
  (define instrs-w/o-nops (remove-nops instrs))
  (debugln "New instrs")
  (debugln instrs-w/o-nops)
  (define fout (open-output-file fn #:exists 'replace))
  (for ([insn instrs-w/o-nops])
    (let ([bytes-out (bv->bytes (riscv-instr->raw insn))])
      (write-bytes bytes-out fout)))
  (close-output-port fout))
