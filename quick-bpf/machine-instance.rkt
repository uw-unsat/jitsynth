#lang rosette

(require "../bpf/machine-instance.rkt")

(provide make-quick-bpf-machine)

(define (make-quick-bpf-machine)
  (make-bpf-machine
    '(addi ldxw ja)
    #:rwset-cache "quick-bpf/rwset-cache"))
