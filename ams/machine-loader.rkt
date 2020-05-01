#lang rosette

(require "../common/data-utils.rkt"
         "../common/file-utils.rkt"
         "../common/data-structures.rkt"
         "../gensketch/rwsets.rkt"
         "../common/options.rkt"
         "machine.rkt")

(require (only-in "../common/debug.rkt" [debugln common:debugln]))

(require rosette/lib/angelic)

(provide make-instruction make-machine)

(define DEBUG #f)

(define (debugln x)
  (if DEBUG (common:debugln x) (void)))

(define (make-instruction
          name
          constructor
          param-acc-funcs
          param-value-funcs
          param-types
          param-bitlens
          [param-cachable-list (make-list (length param-value-funcs) #f)]
          [param-names #f])
  (set! param-names
    (if param-names param-names
      (build-list (length param-value-funcs)
                  (lambda (n) (string->symbol (~a name "-" n))))))
  (define params
    (for/list ([p-name param-names]
               [p-type param-types]
               [p-bitlen param-bitlens]
               [acc param-acc-funcs]
               [choices param-value-funcs]
               [cachable? param-cachable-list])
      (parameter p-name p-type p-bitlen acc choices cachable? #f)))

  (instruction name constructor params #f #f))

(define (make-machine default-state iws instrs nopi
                      ; NOTE: Only required for target languages
                      [instr->getters #f]
                      [rwset-dir #f]
                      #:state-asmt-func [state-asmt-func void]
                      #:state-invar-func [state-invar-func void])
  (let* ([am (machine default-state void void iws instrs nopi instr->getters
                      #f #f #f #f #f)]
         [write-sets
           (cond
             [(not (RWSETS?))
              (for/hash ([i instrs])
                (values (instruction-name i)
                        '(regs mem pc)))]
             [rwset-dir
               (let ([ws (read-from-dir rwset-dir "write-sets")])
                 (if ws ws
                   (write-to-dir rwset-dir "write-sets"
                     (compute-write-sets am))))]
             [else (compute-write-sets am)])]
         [read-sets
           (cond
             [(not (RWSETS?))
              (for/hash ([i instrs])
                (values (instruction-name i)
                        '(regs mem pc)))]
             [rwset-dir
               (let ([rs (read-from-dir rwset-dir "read-sets")])
                 (if rs rs
                   (write-to-dir rwset-dir "read-sets"
                     (compute-read-sets am write-sets))))]
             [else (compute-read-sets am write-sets)])]
         [param-write-sets
           (cond
             [(not (RWSETS?))
              (for/hash ([i instrs])
                (values (instruction-name i)
                  (for/hash ([param (instruction-params i)])
                    (values
                      (parameter-name param)
                      '(regs mem pc)))))]
             [rwset-dir
               (let ([pws (read-from-dir rwset-dir "param-write-sets")])
                 (if pws pws
                   (write-to-dir rwset-dir "param-write-sets"
                     (compute-param-write-sets am write-sets))))]
             [else (compute-param-write-sets am write-sets)])])
    (make-machine-from-rwsets
      default-state state-asmt-func state-invar-func iws instrs nopi instr->getters
      write-sets read-sets param-write-sets)))

(define (make-machine-from-rwsets
          default-state state-asmt-func state-invar-func iws instrs nopi instr->getters
          write-sets read-sets param-write-sets)
  (let* ([am (machine default-state void void iws instrs nopi instr->getters #f #f #f #f #f)]
         [reg-index (symbv (get-regbits am))]
         [mem-index (symbv (get-membits am))]
         [new-instrs
           (for/list ([instr instrs])
             (instruction
               (instruction-name instr)
               (instruction-make instr)
               (for/list ([param (instruction-params instr)])
                 (parameter
                   (parameter-name param)
                   (parameter-type param)
                   (parameter-bitlen param)
                   (parameter-get param)
                   (parameter-value-func param)
                   (parameter-cachable? param)
                   (hash-ref
                     (hash-ref
                       param-write-sets
                       (instruction-name instr))
                     (parameter-name param))))
               (hash-ref read-sets (instruction-name instr))
               (hash-ref write-sets (instruction-name instr))))])
    (machine default-state state-asmt-func state-invar-func
             iws new-instrs nopi instr->getters (make-hash)
             reg-index mem-index write-sets read-sets)))

(define (compute-param-write-set am instr ws param)
  (param-write-set
    ws
    instr param
    (machine-default-state am)
    (get-regbits am)
    (get-membits am)
    (machine-interpret-with-state am)))

(define (compute-write-set am instr)
  (write-set instr (machine-nopi am)
             (get-regbits am) (get-regs-bitlen am) (get-membits am)
             (machine-interpret-with-state am)))

(define (compute-read-set am instr ws)
  (read-set-from-ws
    ws
    instr (machine-nopi am)
    (get-regbits am) (get-regs-bitlen am) (get-membits am)
    (machine-interpret-with-state am)))

(define (compute-write-sets am)
  (let ([instrs (machine-instrs am)])
    (for/hash ([instr instrs])
      (values (instruction-name instr)
              (compute-write-set am instr)))))

(define (compute-read-sets am write-sets)
  (let ([instrs (machine-instrs am)])
    (for/hash ([instr instrs])
      (values (instruction-name instr)
              (compute-read-set
                am instr
                (hash-ref write-sets (instruction-name instr)))))))

(define (compute-param-write-sets am write-sets)
  (let ([instrs (machine-instrs am)])
    (for/hash ([instr instrs])
      (values (instruction-name instr)
              (for/hash ([param (instruction-params instr)])
                (values
                  (parameter-name param)
                  (compute-param-write-set
                    am instr
                    (hash-ref write-sets (instruction-name instr))
                    param)))))))
