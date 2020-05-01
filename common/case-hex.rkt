#lang rosette

(provide case-op)
(provide case-hex)

; NOTE: May want to add "default" functionality.
(define-syntax-rule (case-op opcode [(hex-code ...) expr ...] ...)
  (let ([tmp opcode])
    (cond [(member tmp (map (lambda (h) (bv h 8)) '(hex-code ...))) expr ...] ...
          [else (error (~a "Unrecognized/unimplemented op code: " tmp))])))

(define-syntax-rule (case-hex num size [(hex-code ...) expr ...] ...)
  (let ([tmp num])
    (cond [(member tmp (map (lambda (h) (bv h size)) '(hex-code ...))) expr ...] ...
          [else (error (~a "Unrecognized/unimplemented op code: " tmp))])))
