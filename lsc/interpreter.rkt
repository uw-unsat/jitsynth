#lang rosette

(require "lsc-dsl.rkt"
         "../common/case-hex.rkt"
         "../common/data-structures.rkt"
         "../common/data-utils.rkt"
         "../ams/machine.rkt")

(require (only-in "../common/debug.rkt" [debugln common:debugln]))

; TODO don't do this
(provide (all-defined-out))

(define DEBUG #f)

(define (debugln x)
  (if DEBUG (common:debugln x) void))

; Name should be 'arg, 'rule, or 'default
; TODO should have separate fields for val to avoid union type?
; TODO should I modify the BPF interpreter in a similar way since some values are allowed to be #f?
; TODO should move this to common/instructions
(struct lsc-instruction (name val) #:transparent)

(define (get-arg argnum argvals)
  (debugln (~a "Argvals: " argvals))
  (case-hex argnum 16 ; TODO change num bits here?
    [(0) (list-ref argvals 0)]
    [(1) (list-ref argvals 1)]
    [(2) (list-ref argvals 2)]
    [(3) (list-ref argvals 3)]
    [(4) (list-ref argvals 4)]
    [(5) (list-ref argvals 5)]))

; NOTE: Assuming op is an actual function for now
; Return true if the argvals pass the arg
(define (pass-arg? lsc-arg argvals)
  ; TODO should probably avoid this list-ref, instead use struct
  (let* ([op (db-api-arg-op lsc-arg)]
         ; TODO I'm reading too much stuff here, should do this more efficiently
         [val (get-arg (db-api-arg-arg lsc-arg) argvals)]
         ; TODO shouldn't have to sign-extend (wrongly assuming 32-bit value here)
         [datum (sign-extend (db-api-arg-datum lsc-arg) (bitvector 64))]
         [res (apply op (list val datum))])
    (debugln (~a "op: " op))
    (debugln (~a "val: " val))
    (debugln (~a "datum: " datum))
    (debugln (~a "res: " res))
    res))

; Return true if argvals pass all args for the rule
(define (pass-args? lsc-args argvals)
  (if (empty? lsc-args) #t
    (let* ([arg (first lsc-args)]
           [next-args (rest lsc-args)])
      (and (pass-arg? arg argvals)
           (pass-args? next-args argvals)))))

(define (pass-rule? rule syscall)
  ; TODO change equivalence function to work with syscall struct
  (equal? syscall (db-api-rule-syscall rule)))

#| NOT CURRENTLY USED ############################################# |#
; Return the designated action value if the rule passes
; Otherwise, return #f
; NOTE: Currently not considering syscall priority
(define (eval rule syscall argvals)
  (let* ([lsc-args (db-api-rule-args rule)]
         [action (db-api-rule-action rule)]
         [pass (and (pass-rule? rule syscall)
                    (pass-args? lsc-args argvals))])
    (if pass action #f)))

; If any rule matches, return the action value of the first rule that matches
; Otherwise, return #f
(define (eval-with-rules rules syscall argvals)
  (if (empty? rules) #f
    (let* ([rule (first rules)]
           [next-rules (rest rules)])
      (or (eval rule syscall argvals)
          (eval-with-rules next-rules syscall argvals)))))

(define (interpret-full lsc-filter-col syscall argvals)
  (let* ([lsc-filter-attr (db-filter-col-attr lsc-filter-col)]
         [default (db-filter-attr-act-default lsc-filter-attr)]
         [lsc-filters (db-filter-col-filters lsc-filter-col)]
         ; NOTE: Not sure if this is how the semantics actually work
         [rules (apply append (map db-filter-rules lsc-filters))]
         [res (eval-with-rules rules syscall argvals)])
    (if res res default)))

(define (filter->instr-seq lsc-filter-col)
  (let* ([lsc-filter-attr (db-filter-col-attr lsc-filter-col)]
         [default (db-filter-attr-act-default lsc-filter-attr)]
         [lsc-filters (db-filter-col-filters lsc-filter-col)]
         ; NOTE: Not sure if this is how the semantics actually work
         [rules (apply append (map db-filter-rules lsc-filters))]
         [arg-lists (map db-api-rule-args rules)])
    (flatten (list
      (for/list ([rule rules] [arg-list arg-lists])
        (list
          (map (lambda (arg) (lsc-instruction 'arg arg)) arg-list)
          (lsc-instruction 'rule rule)))
      (lsc-instruction 'default default)))))
#| ################################################################ |#

(define (interpret-with-state instructions registers memory pc [max-depth (bv -1 64)])
  (interpret instructions (state registers memory pc #f) pc (bv 0 64) max-depth))

; TODO problems with this being a symbolic union?
(define (interpret instructions curr-state base-pc depth max-depth)
  (if (or (bvuge depth max-depth)
          (state-return? curr-state)
          (empty? instructions)
          (ge-instruction-length? instructions (state-pc curr-state) base-pc (bv64 1)))
    curr-state
    (let* ([pc (state-pc curr-state)]
           [instr (read-instruction instructions pc base-pc (bv64 1))]
           [new-pc (bvadd (bv 1 64) pc)]
           [type (lsc-instruction-name instr)]
           [val (lsc-instruction-val instr)]
           [regs (state-rvals curr-state)]
           [mem (state-memory curr-state)]
           [should-return? #f]
           [memptr (read-register regs (bv 2 8))]
           [mloc (lambda (i) (bvadd (bv i 64) memptr))]
           ; Syscall parameter passed as first 8 bytes in mem
           [syscall (read-memory mem (mloc 0) (bv 8 64))]
           ; NOTE: Memory stored in little endian (TODO may want to change at some point)
           ; Arg values will be the next 6 memory locations
           [argvals (for/list ([i (range 1 7)])
                      (read-memory mem (mloc (* 8 i)) (bv 8 64)))]
           ; Return value in x0 reg
           ; Single-rule accumulator value is set in the x1 register (avoids jumps)
           [has-failed-rule? (bveq (bv 1 64) (read-register regs (bv8 1)))]
           [should-return? #f])
      (case type
        [(arg-check)
         (if (not (pass-arg? val argvals))
           (begin
             (debugln "can hit in")
             (set! new-pc (bvadd new-pc (bv64 1))))
           (begin
             (debugln "can hit out")
             (void)))]
        [(arg-fail)
         (set! regs (write-register regs (bv8 1) (bv64 1)))]
        [(rule-check-failed)
         (if has-failed-rule?
           (set! new-pc (bvadd new-pc (bv64 2)))
           (void))]
        [(rule-check-pass)
         (define did-pass-rule? (pass-rule? val syscall))
         (if (not did-pass-rule?)
           (set! new-pc (bvadd new-pc (bv64 1)))
           (void))]
        [(rule-pass)
         (set! regs (write-register regs (bv8 0) (db-api-rule-action val)))
         ; TODO change this to a specific register write
         ;      also want to do this in the other languages
         (set! should-return? #t)]
        [(rule-end)
         (set! regs (write-register regs (bv8 1) (bv64 0)))]
        [(default)
         (set! regs (write-register regs (bv8 0) (sign-extend val (bitvector 64))))
         (set! should-return? #t)]
        [(nop) (void)])
      (interpret
        instructions
        (state regs mem new-pc should-return?)
        base-pc
        (bvadd (bv64 1) depth)
        max-depth))))
