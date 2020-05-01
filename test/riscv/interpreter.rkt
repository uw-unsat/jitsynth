#lang rosette

(require "../../riscv/interpreter.rkt" "../../common/data-structures.rkt"
         "../../common/instructions.rkt" "../../common/symbolic-ops.rkt"
         "test-macros.rkt")

(require "../rv64ui/add.rkt"
         "../rv64ui/srai.rkt"
         "../rv64ui/slt.rkt"
         "../rv64ui/sub.rkt"
         "../rv64ui/srli.rkt"
         "../rv64ui/sra.rkt"
         "../rv64ui/addi.rkt"
         "../rv64ui/and.rkt"
         "../rv64ui/slli.rkt"
         "../rv64ui/subw.rkt"
         "../rv64ui/slti.rkt"
         "../rv64ui/sraiw.rkt"
         "../rv64ui/xor.rkt"
         "../rv64ui/slliw.rkt"
         "../rv64ui/addw.rkt"
         "../rv64ui/andi.rkt"
         "../rv64ui/sllw.rkt"
         "../rv64ui/xori.rkt"
         "../rv64ui/sltu.rkt"
         "../rv64ui/addiw.rkt"
         "../rv64ui/srl.rkt"
         "../rv64ui/sll.rkt"
         "../rv64ui/srliw.rkt"
         "../rv64ui/sltiu.rkt"
         "../rv64ui/sraw.rkt"
         "../rv64ui/or.rkt"
         "../rv64ui/ori.rkt"
         "../rv64ui/srlw.rkt")

(require rackunit)

(require (only-in "../../common/debug.rkt" [debugln common:debugln]))

(provide riscv-interpreter-tests)

(define DEBUG #f)

(define (debugln x)
  (if DEBUG (common:debugln x) x))

(define-test-suite riscv-interpreter-tests
  (run-riscv-add-tests)
  (run-riscv-addi-tests)
  (run-riscv-addiw-tests)
  (run-riscv-addw-tests)
  (run-riscv-and-tests)
  (run-riscv-andi-tests)
  (run-riscv-or-tests)
  (run-riscv-ori-tests)
  (run-riscv-sll-tests)
  (run-riscv-slli-tests)
  (run-riscv-slliw-tests)
  (run-riscv-sllw-tests)
  ; TODO
  ; (run-riscv-slt-tests)
  ; (run-riscv-slti-tests)
  ; (run-riscv-sltiu-tests)
  ; (run-riscv-sltu-tests)
  (run-riscv-sra-tests)
  (run-riscv-srai-tests)
  (run-riscv-sraiw-tests)
  (run-riscv-sraw-tests)
  (run-riscv-srl-tests)
  (run-riscv-srli-tests)
  (run-riscv-srliw-tests)
  (run-riscv-srlw-tests)
  (run-riscv-sub-tests)
  (run-riscv-subw-tests)
  (run-riscv-xor-tests)
  (run-riscv-xori-tests))
