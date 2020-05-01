#lang rosette

(require "../ams/machine.rkt"
         "../common/data-utils.rkt"
         "../common/options.rkt")
(require (only-in "../common/debug.rkt" [debugln common:debugln]))
(require rosette/lib/angelic)

(provide make-gensketch-rws
         make-gensketch-rws-with-value)

(define DEBUG #f)

(define (debugln x)
  (if DEBUG (common:debugln x) void))

(define (make-gensketch-rws target rs ws)
  (let* ([instrs
           (if (RW_PRUNE?)
             (get-instrs-for-rs+ws-write-strict target rs ws)
             (machine-instrs target))]
         [sym-instrs-func (thunk (map make-symbolic-instr instrs))])
    (debugln (~a "Making gensketch for read/write sets:" rs " " ws))
    (debugln (~a "Matching instructions: " (map instruction-name instrs)))
    (if (empty? instrs)
      (begin
        (debugln "No target instructions match the given read & write sets")
        ; Only return the empty program, since no instructions match
        (thunk* null))
      (lambda (compute-length)
        (build-list
          compute-length
          (thunk* (apply choose* (sym-instrs-func))))))))

(define (make-gensketch-rws-with-value target rs ws value)
  (let* ([instrs
           (if (RW_PRUNE?)
             (get-instrs-for-rs+ws-write-strict target rs ws)
             (machine-instrs target))]
         [sym-instrs-func
           (thunk
             (let ([tgt-instr-choice (apply choose* instrs)])
               (for*/all ([tgt-instr-name (instruction-name tgt-instr-choice)]
                          [instr-params (instruction-params tgt-instr-choice)])
                 (let* ([instr-make (instruction-make tgt-instr-choice)])
                   (apply instr-make
                     (for/list ([param instr-params])
                       (for/all ([pv-bitlen (get-param-bitlen param)
                                            #:exhaustive])
                         (let* ([pv ((parameter-value-func param))])
                           (if (and (not (equal? -1 pv-bitlen))
                                    (< pv-bitlen (bv-size value)))
                             (choose*
                               (extract (sub1 (bv-size value)) (- (bv-size value) pv-bitlen) value)
                               (extract (sub1 pv-bitlen) 0 value)
                               pv)
                             (if (not (equal? -1 pv-bitlen))
                               (choose*
                                 (sign-extend value (bitvector pv-bitlen))
                                 (zero-extend value (bitvector pv-bitlen))
                                 pv)
                               pv))))))))))])
    (debugln (~a "Making gensketch for read/write sets:" rs " " ws))
    (debugln (~a "...with value: " value))
    (debugln (~a "Matching instructions: " (map instruction-name instrs)))
    (if (empty? instrs)
      (begin
        (debugln "No target instructions match the given read & write sets")
        ; Only return the empty program, since no instructions match
        (thunk* null))
      (lambda (compute-length)
        (build-list
          compute-length
          (thunk* (sym-instrs-func)))))))
