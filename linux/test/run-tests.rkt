#lang rosette

(require "../../bpf/interpreter.rkt"
         "../../ams/machine.rkt"
         "../../common/data-structures.rkt"
         "../../common/data-utils.rkt"
         "../../common/symbolic-ops.rkt"
         "../../common/instructions.rkt"
         "bpf-tests.rkt"
         "utils.rkt")

(unset-sym)

(define (contains-xadd? program)
  (cond
    [(empty? program) #f]
    [(bveq (bv8 0)
           (bvand
             (bvxor
               (bpf-instr-op (first program))
               (bv8 #xc3))
             (bv8 #b11100111)))
     #t]
    [else (contains-xadd? (rest program))]))

(define failed? #f)
(for ([test linux-bpf-test-cases]
      [test-num (length linux-bpf-test-cases)])
  ; (displayln (~a "T" (add1 test-num)))
  ; (for ([i (linux-test-case-code test)])
  ;   (displayln i))
  (when (not (contains-xadd? (linux-test-case-code test)))
    (let* ([result-state
             (interpret-with-state
               (linux-test-case-code test)
               (make-zerod-registers 12)
               (make-zerod-memory #x10000)
               (bv64 0))]
           [expected (linux-test-case-expected test)]
           [passed?
             (for/and ([reg (hash-keys expected)])
               (bveq (extract 31 0 (read-register (state-regs result-state) (bv reg 4)))
                     (bv32 (hash-ref expected reg))))])
      (when (not passed?)
        (set! failed? #t)
        (displayln (~a "Failed test " (add1 test-num)))
        (displayln (~a "Expected: " (bv32 (hash-ref expected 0))))
        (displayln (~a "Actual: " (extract 31 0 (read-register (state-regs result-state) (bv 0 4)))))))))
(when (not failed?)
  (displayln "Passed all tests"))
