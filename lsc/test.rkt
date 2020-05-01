#lang rosette

(require "lsc-dsl.rkt")
(require "interpreter.rkt")
(require "../common/data-structures.rkt")
(require "../common/data-utils.rkt")
(require "../ams/machine.rkt")

(define (bv32 i) (bv i 32))
(define zero (bv32 0))

(define ACCEPT (bv32 1))
(define KILL (bv32 7))

(define attr (db-filter-attr KILL zero zero zero zero zero))
(define arg (db-api-arg zero bvsgt zero zero zero))
(define rule (db-api-rule ACCEPT zero zero (list arg)))
(define s0 zero)
(define s1 (bv32 1))
(define lsc-filter (db-filter null (list rule)))
(define lsc-filter-col (db-filter-col attr (list lsc-filter)))

(define lsc-regs (make-registers 4))
(define lsc-mem (make-memory 128))
(define-symbolic lsc-pc (bitvector 64))
#|
'(rule-check-failed
   rule-check-pass 
   rule-pass
   rule-end
   arg-decide
   arg-check
   arg-fail
   default))
|#
(define res
  (interpret-with-state
    (list (lsc-instruction 'arg-decide
                           (db-api-arg (bv16 1) bvsgt (bv 5 64) (bv 5 64) (bv 5 64))))
    lsc-regs lsc-mem lsc-pc
    (bv64 1)))
(print-registers (state-regs res))
(state-pc res)
