#lang rosette

(provide #%top-interaction #%app #%top define provide quote bv
         (rename-out [bpf-module-begin #%module-begin]))

(require "transformer.rkt")
(require "../interpreter.rkt")
(require "../../common/data-structures.rkt")

(define-syntax (bpf-module-begin expr)
  (syntax-case expr ()
    [(_ x ...) #'(#%module-begin
                  (define ast (transform-bpf-instr `(,x ...))) ; create a list of commands (list of lists)
                  (define (run #:reg [registers (make-zerod-registers 10)] #:mem [memory (make-zerod-memory 100)])
                    (interpret-with-state ast registers memory))
                  (provide ast)
                  (provide run)
                  )]))

(module reader rosette
  (require syntax/strip-context)
  (require "parser.rkt")
 
  (provide (rename-out [bpf-read read]                       
                       [bpf-read-syntax read-syntax]))
  
  (define (bpf-read in)
    (syntax->datum
     (bpf-read-syntax #f in)))

  (define (bpf-read-syntax src in)
    (define lines (filter (lambda (x) (not (void? x))) (map string->datum (port->lines in))))
    (datum->syntax #f `(module bpf-interp bpf ,@lines))))

#|
List of unsupported features:
* lddw (requires two instructions) and other special loads
* call
* be/le (implemented shallowly to match tests)
* r10 (i.e. the stack)
|#
