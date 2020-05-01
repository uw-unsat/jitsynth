#lang rosette

(require "../ams/machine.rkt" "../ams/machine-loader.rkt"
         "../common/instructions.rkt" "../common/data-utils.rkt"
         "../common/data-structures.rkt" "interpreter.rkt"
         "lsc-dsl.rkt")
(require rosette/lib/angelic)

(provide make-lsc-machine)

(define MAX-BITLEN 64)

(define (wrap-lsc-instr name func)
  (define arity (procedure-arity func))
  (procedure-reduce-arity
    (lambda args
      (lsc-instruction name (apply func args)))
    arity))

(define (wrap-lsc-iv acc-func)
  (lambda (instr)
    (acc-func (lsc-instruction-val instr))))

(define (lsc-op-names)
  '(rule-check-failed
    rule-check-pass
    rule-pass
    rule-end
    arg-check
    arg-fail
    default))

; TODO Why is rule-end having so much issue?
(define (make-lsc-machine
          [lsc-names (lsc-op-names)])
  (define (get-constructor name)
    (case name
      [(rule-check-failed rule-check-pass rule-pass rule-end)
       (wrap-lsc-instr name db-api-rule)]
      [(arg-check arg-fail)
        (lambda (arg op mask datum valid)
          (lsc-instruction
            name
            (db-api-arg arg op mask datum valid)))]
      [(default)
        (lambda (val)
          (lsc-instruction 'default val))]))

  (define (get-accessors name)
    (case name
      [(rule-check-failed rule-check-pass rule-pass rule-end)
        (list
          (wrap-lsc-iv db-api-rule-action)
          (wrap-lsc-iv db-api-rule-syscall)
          (wrap-lsc-iv db-api-rule-strict)
          (wrap-lsc-iv db-api-rule-args))]
      [(arg-check arg-fail)
        (list
          (wrap-lsc-iv db-api-arg-arg)
          (wrap-lsc-iv db-api-arg-op)
          (wrap-lsc-iv db-api-arg-mask)
          (wrap-lsc-iv db-api-arg-datum)
          (wrap-lsc-iv db-api-arg-valid))]
      [(default) (list lsc-instruction-val)]))

  (define (get-gens name [arg-num 0])
    (case name
      [(rule-check-failed rule-check-pass rule-pass rule-end)
       (list
         (thunk (symbv MAX-BITLEN))
         (thunk (symbv MAX-BITLEN))
         (thunk #f)
         (thunk #f))]
      [(arg-check arg-fail)
       (list
          ; TODO why is this 16?
          ; (thunk (apply choose* (build-list 7 bv16)))
          (thunk (bv16 arg-num))
          ; TODO allow for more options
          ; TODO also, make these different choices different instructions (probably)
          ; (thunk (choose* bveq (lambda (a b) (not (bveq a b)))))
          (thunk bveq) ; TODO split up diff ops into diff instructions (can't handle same ops in 1 instr)
          (thunk (bv32 0))
          (thunk (symbv MAX-BITLEN))
          (thunk #f))]
      [(default) (list (thunk (symbv MAX-BITLEN)))]))

  (define (get-cats name)
    (case name
      [(rule-check-failed rule-check-pass rule-pass rule-end)
       '(value value op op)]
      [(arg-check arg-fail)
       '(value op op value op)]
      [(default) '(value)]))

  (define (get-bitlens name)
    (case name
      [(rule-check-failed rule-check-pass rule-pass rule-end)
       (list MAX-BITLEN MAX-BITLEN -1 -1)]
      [(arg-check arg-fail)
       (list 16 -1 -1 MAX-BITLEN -1)]
      [(default) (list MAX-BITLEN)]))

  (define (get-cachables name)
    (case name
      [(rule-check-failed rule-check-pass rule-pass rule-end)
       (make-list 4 #t)]
      [(arg-check arg-fail)
       (make-list 5 #t)]
      [(default) '(#t)]))

  (define (get-arg-names name)
    (case name
      [(rule-check-failed rule-check-pass rule-pass rule-end)
       '(action syscall strict args)]
      [(arg-check arg-fail)
       '(arg op mask datum valid)]
      [(default) '(default)]))

  ; TODO adjust code b/c these are all wrapped in lsc-instruction struct
  ; TODO NOTE: For now, only allowing arg-num = 0
  ;            Can make more args later, but they need different instruction names
  (define arg-num 0)
  (define lsc-instructions
    (for*/list ([name lsc-names]
                ; [arg-num 6] ; TODO
                #:when
                (or (= 0 arg-num)
                    (member name '(arg-check))))
      (make-instruction
        name
        (get-constructor name)
        (get-accessors name)
        (get-gens name arg-num)
        (get-cats name)
        (get-bitlens name)
        (get-cachables name)
        (get-arg-names name))))

  (define lsc-regs (make-registers 3))
  (define lsc-mem (make-memory 128))
  (define-symbolic lsc-pc (bitvector 64))
  (define-values (lsc-machine)
    (make-machine
      (make-state lsc-regs lsc-mem lsc-pc)
      interpret-with-state
      lsc-instructions
      (lsc-instruction 'nop #f)
      #f ; Don't need this argument for source machines
      "lsc/rwset-cache"))

  lsc-machine)
