#lang rosette

(require "bpf2riscv/rwsets.rkt")
(require "bpf/interpreter.rkt")
(require "riscv/interpreter.rkt")

(require rackunit rackunit/text-ui)

(void
  (run-tests riscv-interpreter-tests)
  (run-tests bpf-interpreter-tests)
  (run-tests riscv-write-set-tests)
  (run-tests riscv-read-set-tests)
  (run-tests bpf-write-set-tests)
  (run-tests bpf-read-set-tests))
