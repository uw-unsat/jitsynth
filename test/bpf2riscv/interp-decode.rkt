#lang rosette

(require "../riscv/decoder.rkt"
         "../riscv/interpreter.rkt"
         "../riscv/tostr.rkt"
         "../ams/machine.rkt"
         "../common/data-structures.rkt"
         "../common/instructions.rkt")
(require file/glob)

(require (only-in "../common/debug.rkt" [debugln common:debugln]))

(define DEBUG #t)

(define (debugln x)
  (if DEBUG (common:debugln x) void))

(define dir "linux/compiled/jitk-linux-kaby")
  ; "../riscvisor/monitors/testmon/filters/jitsynth/jitk")
  ; "linux/compiled/jitk-synth-kaby")
(define file-names (glob (~a dir "/*.rv.bin")))
(for ([fn file-names])
  (debugln (~a "File " fn))
  (define prog (decode-file fn))
  #|
  (for ([i prog] [pc (map (lambda (x) (bv (* 4 x) 64)) (range 0 (length prog)))])
    (debugln (~a "PC: " pc
                 "\nHex: " (riscv-instr->raw i)
                 "\nInstr: " (tostr-instr i)
                 "\n" i "\n"))))
  |#
  (define input-mem
    (write-memory (make-zerod-memory 128) (bv 4 64) (bv 4 64) (bv #xC000003E 32)))
  (define regs
    (state-regs
      (interpret-with-state
        prog
        (write-register
          (make-zerod-registers 32)
          (bv 1 5)
          (bv -1 64))
        input-mem
        (bv 0 64))))
  (debugln (~a "Return value: " (read-register regs (bv 15 5)))))
