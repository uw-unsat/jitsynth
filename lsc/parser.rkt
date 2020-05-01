#lang rosette

(require "lsc-dsl.rkt")
(require "interpreter.rkt")
(require "../common/data-structures.rkt")
(require "../common/data-utils.rkt")
(require "../ams/machine.rkt")

(require file/glob)

(define (get-value name)
  (case name
    ; TODO
    [(stuff) (bv64 5)]))

; ARG argnum op datum
; RULE syscall action
; ...
; DEFAULT action

(define (parse-op op-name)
  (case op-name
    [(eq) bveq]
    [(ne) (lambda (a b) (not (bveq a b)))]
    [(sgt) bvsgt]
    [(sge) bvsge]
    [(slt) bvslt]
    [(sle) bvsle]
    [(ugt) bvugt]
    [(uge) bvuge]
    [(ult) bvult]
    [(ule) bvule]
    [else (thunk* #t)]))

(define (parse-action action)
  (bv64
    (case action
      [(ALLOW) 0]
      [(KILL) 1]
      [(DENY) 2]
      [else -1])))

(define (parse-syscall syscall)
  (bv64
    (case syscall
      ; TODO
      [(rt_sigreturn) 173]
      [(sigreturn) 119]
      [(madvise) 200]
      [(mmap) 10]
      [(brk) 13]
      [(munmap) 91]
      [(mprotect) 125]
      [(mremap) 163]
      [(futex) 201]
      [(mmap2) 202]
      [(exit) 1]
      [(read) 3]
      [(write) 4]
      [else -1])))

(define (parse-argnum argnum)
  #F)
  ; TODO 
  ; (bv16 (string->number (symbol->string argnum))))

(define (parse-number num)
  #F)
  ; TODO
  ; (bv64 (string->number num)))

(define (parse-line line)
  (define args
    (map
      string->symbol
      (string-split line)))
  (case (first args)
    [(ARG) (make-arg (parse-argnum (second args))
                     (parse-op (third args))
                     (parse-number (fourth args)))]
    [(RULE) (make-rule (parse-syscall (second args))
                       (parse-action (third args)))]
    [(DEFAULT) (make-default (parse-action (second args)))]))

(define (make-arg arg op datum)
  (map (lambda (name) (lsc-instruction name (db-api-arg arg op #f datum #f)))
       '(arg-check arg-fail)))

(define (make-rule action syscall)
  (map (lambda (name) (lsc-instruction name (db-api-rule action syscall #f null)))
       '(rule-check-failed rule-check-pass rule-pass rule-end)))

; TODO map values before or after?
(define (make-default action)
  (list (lsc-instruction 'default action)))

#|
; Basic preliminary computations
; Helper for computing output length
(define (js-ebpf-size-map lsc-instr)
  (case (lsc-instruction-name lsc-instr)
    [(rule-check-failed rule-check-pass) 3]
    [(rule-pass arg-check default) 2]
    [(rule-end arg-fail) 1]))

; Helper for computing output length (linux)
(define (linux-obpf-size-map lsc-instr)
  (case (lsc-instruction-name lsc-instr)
    [(rule-check-failed rule-check-pass rule-pass rule-end) 0.5]
    [(arg-check arg-fail) 3.5]
    [(default) 1]))
|#

; Code for counting program length
(define dir "lsc/benchmarks")
(define file-names (glob (~a dir "/*.lsc")))
(for ([fn file-names])
  (displayln (~a "At file " fn))
  (define fin (open-input-file fn))
  (define fout (open-output-file (~a fn ".ops") #:exists 'replace))
  (define lsc-prog
    (let f ()
      (let ([line (read-line fin)])
        (if (eof-object? line)
          null
          (append (parse-line line) (f))))))
  (displayln (length lsc-prog) fout)
  (for ([instr lsc-prog])
    (displayln 
      (case (lsc-instruction-name instr)
        [(rule-check-failed) 0]
        [(rule-check-pass) 1]
        [(rule-pass) 2]
        [(rule-end) 3]
        [(arg-check) 4]
        [(arg-fail) 5]
        [(default) 6])
      fout))
  (close-output-port fout)
  (close-input-port fin))
