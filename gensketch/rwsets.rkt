#lang rosette

(require "../common/data-structures.rkt")
; (require "../common/state.rkt")
(require "../common/data-utils.rkt")
(require "../ams/machine.rkt")
(require (only-in "../common/debug.rkt" [debugln common:debugln]))
(require rosette/lib/angelic)

(provide write-set)
(provide read-set)
(provide read-set-from-ws)
(provide param-write-set)
(provide unique-reg-read-set)

(define DEBUG #f)

(define (debugln x)
  (if DEBUG (common:debugln x) (void)))

#| ############### EQUALITY METHODS ############### |#
; Need to define a NOP operation to determine whether things were written to or not
(define (pc-eq? PC1 PC2)
  (unsat?
    (verify (assert (bveq PC1 PC2)))))

(define (reg-eq? regs1 regs2 regbits)
  (define-symbolic* ind (bitvector regbits))
  (let ([res
          (verify (assert (implies
            (bvult (zero-extend ind (bitvector 8)) (numregs regs1))
            (bveq (read-register regs1 ind) (read-register regs2 ind)))))])
    (unsat? res)))

(define (mem-eq? mem1 mem2 membits)
  (define-symbolic* ind (bitvector membits))
  (let ([res (verify (assert (bveq
          (read-memory mem1 ind (bv 1 64))
          (read-memory mem2 ind (bv 1 64)))))])
    (unsat? res)))

(define (return-eq? ret1 ret2)
  (unsat?
    (verify (assert (equal? ret1 ret2)))))

#| ############### READ/WRITE SET COMPUTATION ############### |#
; Things that can be in the write-set are 'PC, 'regs, and 'mem

; instr:                   instruction whose write set will be calculated
; nopi:                    nop in desired language 
; regbits:                 number of bits in the register index
; membits:                 number of bits in the memory index
; interpret-with-state:    the interpreter function for the language
(define (write-set am-instr nopi regbits reg-bitlen membits interpret-with-state)
  (define instr (make-symbolic-instr am-instr))
  (debugln (~a "Computing write set for s-instr: " instr))

  (define start-PC (symbv 64))
  (define start-regs (make-registers (expt 2 regbits) reg-bitlen))
  (define start-mem (make-memory (expt 2 membits)))

  ; Run a nop to determine what the "idle" transformation is
  (define nop-state
    (interpret-with-state (list nopi) start-regs start-mem start-PC (bv 1 64)))
  (debugln "Done interpreting nop")
  (define nop-PC
    (state-pc nop-state))
  (define nop-regs
    (state-regs nop-state))
  (define nop-mem
    (state-mem nop-state))
  (define nop-return
    (state-return? nop-state))
  (let* ([end-state
           (interpret-with-state (list instr) start-regs start-mem start-PC (bv 1 64))]
         [end-PC (state-pc end-state)]
         [end-regs (state-regs end-state)]
         [end-mem (state-mem end-state)]
         [end-return (state-return? end-state)]
         [ws null])
    (debugln "Register values in computing write set")
    (debug-registers end-regs DEBUG)
    (debugln "Return? value in computing write set")
    (debugln end-return)
    ; NOTE: This is a SMT query, will return #t if always equal, #f if possibly not equal
    (if (not (pc-eq? nop-PC end-PC))
      (set! ws (cons 'pc ws)) (void))
    (if (not (reg-eq? nop-regs end-regs regbits))
      (set! ws (cons 'regs ws)) (void))
    (if (not (mem-eq? nop-mem end-mem membits))
      (set! ws (cons 'mem ws)) (void))
    (if (not (return-eq? nop-return end-return))
      (set! ws (cons 'return? ws)) (void))
    ws))

(define (states-differ? ws s1 s2 regbits membits)
  (or (and (member 'pc ws) (not (pc-eq? (state-pc s1) (state-pc s2))))
      (and (member 'regs ws) (not (reg-eq? (state-regs s1) (state-regs s2) regbits)))
      (and (member 'mem ws) (not (mem-eq? (state-mem s1) (state-mem s2) membits)))
      (and (member 'return? ws) (not (return-eq? (state-return? s1) (state-return? s2))))))

(define (states-differ-exp ws s1 s2 regbits membits testing-mem-index)
  (not (equal? (state-return? s1) (state-return? s2))))

(define (pc-read? instr ws regbits reg-bitlen membits interpret-with-state)
  (define-symbolic pc1 (bitvector 64))
  (define-symbolic pc2 (bitvector 64))
  (define start-regs (make-registers (expt 2 regbits) reg-bitlen))
  (define start-mem (make-memory (expt 2 membits)))
  (let ([res1 (interpret-with-state (list instr) start-regs start-mem pc1 (bv 1 64))]
        [res2 (interpret-with-state (list instr) start-regs start-mem pc2 (bv 1 64))])
    (states-differ? ws res1 res2 regbits membits)))

(define (reg-read? instr ws regbits reg-bitlen membits interpret-with-state)
  (define regs1 (make-registers (expt 2 regbits) reg-bitlen))
  (define regs2 (make-registers (expt 2 regbits) reg-bitlen))
  (define start-mem (make-memory (expt 2 membits)))
  (define-symbolic start-PC (bitvector 64))
  (let ([res1 (interpret-with-state (list instr) regs1 start-mem start-PC (bv 1 64))]
        [res2 (interpret-with-state (list instr) regs2 start-mem start-PC (bv 1 64))])
    (states-differ? ws res1 res2 regbits membits)))

(define (mem-read? instr ws regbits reg-bitlen membits interpret-with-state)
  (define start-regs (make-registers (expt 2 regbits) reg-bitlen))
  (define mem1 (make-memory (expt 2 membits)))
  (define mem2 (make-memory (expt 2 membits)))
  (define-symbolic start-PC (bitvector 64))
  (let ([res1 (interpret-with-state (list instr) start-regs mem1 start-PC (bv 1 64))]
        [res2 (interpret-with-state (list instr) start-regs mem2 start-PC (bv 1 64))])
    (states-differ? ws res1 res2 regbits membits)))

(define ENABLE-MEM-INDEX-READ-SET? #f)
(define (mem-read-index?
          instr ws regbits reg-bitlen membits
          interpret-with-state [prev-pairs empty])
  (define start-regs (make-registers (expt 2 regbits) reg-bitlen))
  (debugln (~a "Regbits: " regbits))
  (define rind (symbv regbits))
  (define rval (read-register start-regs rind))
  (define memoff (symbv 64))
  (debugln (~a "rval,memoff: " rval "," memoff))
  (define testing-mem-index (bvadd rval memoff))
  (define basemem (make-memory (expt 2 membits)))
  (define mem1 (write-memory basemem testing-mem-index (bv64 1) (symbv 8)))
  (define mem2 (write-memory basemem testing-mem-index (bv64 1) (symbv 8)))
  (debugln (~a "Updated mem1: " (read-memory mem1 testing-mem-index (bv64 1))))
  (debugln (~a "Updated mem2: " (read-memory mem2 testing-mem-index (bv64 1))))
  (define-symbolic start-PC (bitvector 64))
  (define res1 (interpret-with-state (list instr) start-regs mem1 start-PC (bv 1 64)))
  (define res2 (interpret-with-state (list instr) start-regs mem2 start-PC (bv 1 64)))
  (debugln (~a "Instr: " instr))
  (debugln (~a "Previous pairs: " prev-pairs))
  (define soln
    (complete-solution
      (synthesize
        #:forall (list start-PC (base basemem))
        #:assume (for ([pair prev-pairs])
                   (define prev-rind (first pair))
                   (define prev-memoff (second pair))
                   (assert (not (and (bveq prev-rind rind)
                                     (bveq prev-memoff memoff)))))
        #:guarantee (assert (states-differ-exp ws res1 res2 regbits membits testing-mem-index)))
      (list rind memoff)))
  (debugln soln)
  (debugln (~a "Soln: " (if (unsat? soln) #f (list (evaluate rind soln) (evaluate memoff soln)))))
  (if (unsat? soln) #f (list (evaluate rind soln) (evaluate memoff soln))))

(define (all-mem-read-indexes
          instr ws regbits reg-bitlen membits
          interpret-with-state [prev-pairs empty])
  (define new-pair
    (mem-read-index? instr ws regbits reg-bitlen membits interpret-with-state prev-pairs))
  (if new-pair
    (all-mem-read-indexes
      instr ws regbits reg-bitlen membits
      interpret-with-state (cons new-pair prev-pairs))
    prev-pairs))

(define (read-set am-instr nopi regbits reg-bitlen membits interpret-with-state)
  (let ([ws (write-set am-instr nopi regbits reg-bitlen membits interpret-with-state)])
    (read-set-from-ws ws am-instr nopi regbits reg-bitlen membits interpret-with-state)))
    
(define (read-set-from-ws ws am-instr nopi regbits reg-bitlen membits interpret-with-state)
  (define instr (make-symbolic-instr am-instr))
  (debugln (~a "Computing read set for s-instr: " instr))
  (let ([rs null])
    (if (pc-read? instr ws regbits reg-bitlen membits interpret-with-state)
      (set! rs (cons 'pc rs)) (void))
    (if (reg-read? instr ws regbits reg-bitlen membits interpret-with-state)
      (set! rs (cons 'regs rs)) (void))
    (if (mem-read? instr ws regbits reg-bitlen membits interpret-with-state)
      (set! rs (cons 'mem rs)) (void))
    (if ENABLE-MEM-INDEX-READ-SET?
      (for ([pair (all-mem-read-indexes instr ws regbits reg-bitlen membits interpret-with-state)])
        (define-values (rind memoff)
          (values
            (bitvector->natural (first pair))
            (bitvector->integer (second pair))))
        (set! rs (cons (string->symbol (~a rind "+" memoff)) rs)) (void))
      (void))
    rs))

#| ############### PARAMETER CORRESPONDENCE ############### |#
; NOTE: If the parameter write set is empty, then it is not read. 
(define (param-write-set instr-ws am-instr write-param default-state
                         regbits membits interpret-with-state)
  (let* ([param-vals (map
                       (lambda (p)
                         ((parameter-value-func p)))
                       (instruction-params am-instr))]
         [alt-val ((parameter-value-func write-param))]
         [instr1 (apply (instruction-make am-instr) param-vals)]
         [instr2
           (apply (instruction-make am-instr)
                  (for/list ([param (instruction-params am-instr)]
                             [i (length (instruction-params am-instr))])
                    (if (equal? (parameter-name write-param) (parameter-name param))
                      alt-val (list-ref param-vals i))))]
         [res1 (interpret-with-state
                 (list instr1)
                 (state-regs default-state)
                 (state-mem default-state)
                 (state-pc default-state)
                 (bv 1 64))]
         [res2 (interpret-with-state
                 (list instr2)
                 (state-regs default-state)
                 (state-mem default-state)
                 (state-pc default-state)
                 (bv 1 64))]
         [ws null])
    (if (and (member 'pc instr-ws)
             (not (pc-eq? (state-pc res1) (state-pc res2))))
      (set! ws (cons 'pc ws)) (void))
    (if (and (member 'regs instr-ws)
             (not (reg-eq? (state-regs res1) (state-regs res2) regbits)))
      (set! ws (cons 'regs ws)) (void))
    (if (and (member 'mem instr-ws)
             (not (mem-eq? (state-mem res1) (state-mem res2) membits)))
      (set! ws (cons 'mem ws)) (void))
    ws))

#| ############### UNIQUE REGISTER READ SET ############### |#
; NOTE: These functions machine has already been constructed
(define (unique-reg-read-set am-instr am)
  (define instr (make-symbolic-instr am-instr))
  (define regbits (get-regbits am))
  (define membits (get-membits am))
  (define interpret-with-state (machine-interpret-with-state am))
  (define reg-bitlen (get-regs-bitlen am))
  (define regs1 (make-registers (expt 2 regbits) reg-bitlen))
  (define start-mem (make-memory (expt 2 membits)))
  (define-symbolic start-PC (bitvector 64))
  (define poss-regs
    (build-list (get-numregs am) bv8))
  (filter 
    (lambda (regind)
      (define regs2 (write-register regs1 regind (symbv reg-bitlen)))
      (let ([res1 (interpret-with-state (list instr) regs1 start-mem start-PC (bv 1 64))]
            [res2 (interpret-with-state (list instr) regs2 start-mem start-PC (bv 1 64))])
        (states-differ?
          (get-write-set am-instr)
          res1 res2 regbits membits)))
    poss-regs))
