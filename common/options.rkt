#lang rosette

(require "debug.rkt")
(require rosette/solver/smt/boolector)
(require rosette/solver/smt/z3)

(provide MINPROGLEN
         MAXPROGLEN
         COMPUTE_BOUND
         THREADS
         SLEEPTIME
         OVERALL_TIMEOUT
         QUERY_TIMEOUT
         LS_REORDER?
         PARAM_CORR?
         RW_PRUNE?
         USE_RETURN?
         RWSETS?
         set-optimization-options
         set-synthesis-options
         set-timeouts
         set-use-return
         set-solver)

; For synthesis/compiler-query
(define MINPROGLEN 1)
(define MAXPROGLEN 9)
(define COMPUTE_BOUND 3)
(define THREADS 3)
(define SLEEPTIME 15)

; Timeouts in millis
; 1000 = 1 second
; -1 = no timeout

; Overall timeout for creating compiler, no timeout by default
(define OVERALL_TIMEOUT (* 2 24 60 60 1000)) ; 48 hours

; Timeout for a single query, no timeout by default
(define QUERY_TIMEOUT -1)

; For gensketch/gensketch
; Whether or not to include optimizations
(define LS_REORDER?-const #t)
(define PARAM_CORR?-const #t)
(define RW_PRUNE?-const #t)
(define RWSETS?-const #t)

(define (LS_REORDER?)
  ; (debugln (~a "LS_REORDER?: " LS_REORDER?-const))
  LS_REORDER?-const)

(define (PARAM_CORR?)
  ; (debugln (~a "PARAM_CORR?: " PARAM_CORR?-const))
  PARAM_CORR?-const)

(define (RW_PRUNE?)
  ; (debugln (~a "RW_PRUNE?: " RW_PRUNE?-const))
  RW_PRUNE?-const)

(define (RWSETS?)
  RWSETS?-const)

; Option for reasoning about returned state
(define USE_RETURN? #t)

; Solver to use. Options: 'z3, 'boolector
(define SOLVER_NAME 'boolector)

(define (update-current-solver)
  (case SOLVER_NAME
    [(boolector)
     (current-solver (boolector #:path "../boolector/"))]
    [(z3)
     (current-solver (z3))]
    [else (error (~a "Invalid solver name: " SOLVER_NAME))]))

; Set solver based on default options first
(update-current-solver)

(define (set-optimization-options ls-reorder? param-corr? rw-prune? rwsets?)
  (set!-values
    (LS_REORDER?-const PARAM_CORR?-const RW_PRUNE?-const RWSETS?-const)
    (values ls-reorder? param-corr? rw-prune? rwsets?)))

(define (set-synthesis-options minproglen maxproglen threads sleeptime)
  (set!-values
    (MINPROGLEN MAXPROGLEN THREADS SLEEPTIME)
    (values minproglen maxproglen threads sleeptime)))

(define (set-timeouts overall-timeout query-timeout)
  (set!-values
    (OVERALL_TIMEOUT QUERY_TIMEOUT)
    (values overall-timeout query-timeout)))

(define (set-use-return use-return?)
  (set! USE_RETURN? use-return?))

(define (set-solver solver-name)
  (set! SOLVER_NAME solver-name)
  (update-current-solver))
