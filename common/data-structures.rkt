#lang rosette

(require "case-hex.rkt"
         "data-utils.rkt")

(require (only-in "debug.rkt" [debugln common:debugln]))

(provide make-zerod-memory make-memory make-indef-memory
         read-memory write-memory
         make-zerod-registers make-registers
         read-register numregs regs-bitlen
         mem-size hardwire-register
         write-register read-instruction
         ge-instruction-length?  pc-out-of-bounds?
         make-store-from-existing make-memory-from-list
         print-registers debug-registers base)

(define DEBUG #f)

(define (debugln x)
  (if DEBUG (common:debugln x) void))

(define bv0-7 (build-list 8 bv64))

; NOTE: Assuming size = 1/2/4/8
; Assume size is bv64
(define (bv-list size)
  (case-hex size 64
            [(1) (take bv0-7 1)]
            [(2) (take bv0-7 2)]
            [(4) (take bv0-7 4)]
            [(8) (take bv0-7 8)]))

; Assume value and index are bv64
(define (byte-at value index)
  (case-hex index 64
            [(0) (extract 7 0 value)]
            [(1) (extract 15 8 value)]
            [(2) (extract 23 16 value)]
            [(3) (extract 31 24 value)]
            [(4) (extract 39 32 value)]
            [(5) (extract 47 40 value)]
            [(6) (extract 55 48 value)]
            [(7) (extract 63 56 value)]))

(define default-hardwired (lambda (index) #f))

(struct store (size func base hardwired input-bitlen output-bitlen) #:transparent)

(define (make-zerod-memory size)
  (let ([size (bv64 size)]
        [func (lambda (index)
                ; Unclear if this assertion and others are necessary
                ; (assert (bvult index size))
                (bv 0 8))])
    (store size func func default-hardwired 64 8)))

(define (make-zerod-registers size [bitlen 64])
  (let ([size (bv size 8)]
        [func (lambda (index)
                ; (assert (bvult index size))
                (bv 0 bitlen))])
    (store size func func default-hardwired 8 bitlen)))

; Assuming index is bv64
; TODO remove size, since I don't actually use it
(define (make-memory size)
  (let ([size (bv64 size)])
    (define-symbolic* membase (~> (bitvector 64) (bitvector 8)))
    (store
     size
     (lambda (index)
       ; (assert (bvult index size))
       (apply membase (list index)))
     membase
     default-hardwired
     64 8)))

(define (make-indef-memory)
  (make-memory -1))

; NOTE: Only to be used by testing framework
(define (make-memory-from-list l)
  (let ([size (bv64 (length l))])
    (store
     size
     (lambda (index)
       ; (assert (bvult index size))
       (let ([index/int (bitvector->natural index)])
         (list-ref l index/int)))
     #f
     default-hardwired
     64 8)))

; Assuming index is bv8
(define (make-registers size [bitlen 64])
  (let ([size (bv size 8)])
    (define-symbolic* regbase (~> (bitvector 8) (bitvector bitlen)))
    (store
     size
     (lambda (index)
       ; (assert (bvult index size))
       (apply regbase (list index)))
     regbase
     default-hardwired
     8 bitlen)))

(define (make-store-from-existing
          st indmap size
          [input-bitlen (store-input-bitlen st)]
          [output-bitlen (store-output-bitlen st)])
  (let ([size (bv size input-bitlen)]
        [func (store-func st)])
    ; NOTE: These assertions should only include actual constants, not symbolic constants
    (assert (<= (store-input-bitlen st) input-bitlen))
    (assert (<= output-bitlen (store-output-bitlen st)))
    (store
     size
     (lambda (index)
       (define new-index (zero-extend index (bitvector input-bitlen)))
       (extract (sub1 output-bitlen) 0
                (apply func (list (apply indmap (list index))))))
     (store-base st)
     default-hardwired
     input-bitlen
     output-bitlen)))

(define (read-elem st index)
  (apply (store-func st) (list index)))

(define (write-elem st index output)
  (let ([hw? (store-hardwired st)])
    (if (hw? index) st 
        (store
         (store-size st)
         (lambda (i)
           ; (assert (bvult i (store-size st)))
           (if (bveq i index) output (read-elem st i)))
         (store-base st)
         hw?
         (store-input-bitlen st)
         (store-output-bitlen st)))))

(define (hardwire-elem st index output)
  (let ([hw? (store-hardwired st)])
    (store
     (store-size st)
     (lambda (i)
       ; (assert (bvult i (store-size st)))
       (if (bveq i index) output (read-elem st i)))
     (store-base st)
     (lambda (i) (if (bveq i index) #t (hw? index)))
     (store-input-bitlen st)
     (store-output-bitlen st))))

; Memory stored in little endian, so read bytes in reverse order
; TODO change bytes to my-num-bytes
(define (read-memory memory index num-bytes)
  (let* ([index (zero-extend index (bitvector 64))]
         [memlen (store-size memory)]
         [bytes (zero-extend num-bytes (bitvector 64))])
    ; (assert (bvult index memlen))               ; index < memlen
    ; (assert (bvule (bvadd index bytes) memlen)) ; index + bytes <= memlen
    (debugln (~a "Bytes: " bytes))
    (assert (or (bveq bytes (bv64 1)) (bveq bytes (bv64 2))
                (bveq bytes (bv64 4)) (bveq bytes (bv64 8)))) ; bytes = 1/2/4/8
    (apply concat
           (map (lambda (i) (read-elem memory (bvadd index i)))
                (reverse (bv-list bytes))))))

(define (write-memory memory index num-bytes value)
  (let* ([index (zero-extend index (bitvector 64))]
         [value (sign-extend value (bitvector 64))]
         [memlen (store-size memory)]
         [bytes (zero-extend num-bytes (bitvector 64))])
    (debugln (~a "Index: " index))
    (debugln (~a "Memlen: " memlen))
    (debugln (~a "Bytes: " bytes))
    ; (assert (bvult index memlen))               ; index < memlen
    ; (assert (bvule (bvadd index bytes) memlen)) ; index + bytes <= memlen
    (assert (or (bveq bytes (bv64 1)) (bveq bytes (bv64 2))
                (bveq bytes (bv64 4)) (bveq bytes (bv64 8)))) ; bytes = 1/2/4/8
    ; The efficiency of this implementation is unclear
    (foldl
     (lambda (i mem)
       (write-elem mem (bvadd index i) (byte-at value i)))
     memory
     (bv-list bytes))))

(define (numregs registers)
  (store-size registers))

(define (regs-bitlen registers)
  (store-output-bitlen registers))

(define (mem-size memory)
  (store-size memory))

; TODO: NEXT why is this being passed a 64-bit value?
(define (read-register registers index)
  (let* ([index (zero-extend index (bitvector 8))]
         [regs (store-size registers)])
    (assert (bvult index regs))
    (read-elem registers index)))

(define (write-register registers index value)
  (let* ([index (zero-extend index (bitvector 8))]
         [regs (store-size registers)])
    (assert (bvult index regs))
    (write-elem registers index value)))

(define (hardwire-register registers index value)
  (let* ([index (zero-extend index (bitvector 8))]
         [regs (store-size registers)])
    (assert (bvult index regs))
    (hardwire-elem registers index value)))

(define (base st) (store-base st))

(define (debug-registers st ext-DEBUG [mdl #f])
  (if ext-DEBUG (print-registers st mdl) (void)))

(define (print-registers st [mdl #f])
  (let ([size/int (bitvector->natural (store-size st))])
    (for ([i (in-range size/int)])
      (displayln (~a "x" i ": "
                     (if (not mdl)
                       (read-elem st (bv i 8))
                       (evaluate (read-elem st (bv i 8)) mdl)))))
    (displayln "")))

; TODO remove
(define (simplify-bvadd x y)
  (debugln (~a "SBVADD: " x ", " y))
  (match* (x y)
    [((bv c1 l) (expression (== bvadd) (bv c2 l) n))
     (begin
       (bvadd (bvadd (bv c1 l) (bv c2 l)) n))]
    [(a b)
     (begin
       (bvadd a b))]))

; TODO try to put this for/all in other places as well
; Either want to push all the conditionals to the top or bottom (probably top)
(define (pc-add off my-pc)
  (debugln "\nPC ADD")
  (debugln (~a "Asserts: " (asserts)))
  (debugln (~a "Off & PC: " off ", " my-pc))
  (debugln (~a "Path Cond: " pc))
  (define res ; (simplify-bvadd off pc))
    (for/all ([p my-pc #:exhaustive])
      (begin
        (debugln (~a "Subvalue: " p))
        (simplify-bvadd off p))))
  (debugln (~a "Result: " res))
  (debugln (~a "Asserts: " (asserts)))
  (debugln "PC ADD DONE\n")
  res)

(define (pc-cond cond-func num my-pc)
  (debugln (~a "Cond: " cond-func))
  (debugln (~a "PC: " my-pc))
  (debugln (~a "Num: " num))
  (define res
    (for/all ([p my-pc #:exhaustive])
      (apply cond-func (list num p))))
  (debugln (~a "Res: " res))
  res)

; NOTE: PC is 64-bit
(define (read-instr-helper instructions relpc off)
  (debugln (~a "Relpc: " relpc))
  ; TODO put this in a different function?
  (if (bvuge relpc (bvmul off (bv64 (length instructions))))
    (error "pc out of bounds")
    (if (pc-cond bveq (bv64 0) relpc)
      (begin
        (debugln (~a "Hit RPC: " relpc))
        (first instructions))
      (read-instr-helper (rest instructions) (pc-add (bvneg off) relpc) off))))

(define (read-instruction instructions pc base off)
  (read-instr-helper instructions (bvsub pc base) off))

(define (ge-instruction-length? instructions pc base off)
  (pc-cond bvule (bvmul off (bv64 (length instructions))) (bvsub pc base)))

(define (pc-out-of-bounds? instructions pc base off)
  (or 
    (pc-cond bvule (bvmul off (bv64 (length instructions))) (bvsub pc base))))
    ; (pc-cond bvult pc base)))
