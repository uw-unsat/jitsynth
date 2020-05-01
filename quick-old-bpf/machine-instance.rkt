#lang rosette

(require "../old-bpf/machine-instance.rkt")

(provide make-quick-old-bpf-machine)

(define (make-quick-old-bpf-machine)
  (make-old-bpf-machine
    '(add mul tax ld st)
    #:rwset-cache "quick-old-bpf/rwset-cache"))
