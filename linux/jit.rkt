#lang rosette

(require "../common/instructions.rkt")
(require (rename-in "../common/interp-utils.rkt" (assemble-and-run aar)))

(provide assemble)
; TODO remove this, move to tester side
(provide assemble-and-run)

; NOTE: May want debug option
(define (assemble bpf-instrs)
  (let ([bpf-prog (map bpf-instr->raw bpf-instrs)])
    ; Run jit and obtain input/output ports
    ; Requires running from base directory
    (define ports (process "./linux/bpf_riscv_jit stdin stdout"))
    (define stdout (list-ref ports 0))
    (define stdin (list-ref ports 1))
    (define stderr (list-ref ports 3))
    
    ; Print bpf program to stdin
    (displayln (length bpf-prog) stdin)
    (for ([insn bpf-prog])
      ; Don't worry about leading zeros for now, C compiler doesn't need them
      (displayln (number->string (bitvector->natural insn) 16) stdin))
    (close-output-port stdin)

    ; (displayln (read-line stderr))
    ; Read RISC-V program from stdout
    (define (read-file)
      (let ([line (read-line stdout)])
        ; (displayln line)
        (if (eof-object? line) null
          (cons (raw->riscv-instr (bv (string->number line 16) 32))
                (read-file)))))
    (define output (read-file))
    (close-input-port stdout)
    (close-input-port stderr)
    ; (pretty-print output)
    output))

(define (assemble-and-run bpf-instrs regs mem)
  (aar assemble bpf-instrs regs mem))
