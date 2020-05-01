#lang rosette

(provide smul sdiv smod
         smul32 sdiv32 smod32
         symbolic-bvops
         set-sym unset-sym)

(define SYM-FUNC? #t)

(define (set-sym)
  (set! SYM-FUNC? #t)
  (set-funcs)
  (set-bvops))

(define (unset-sym)
  (set! SYM-FUNC? #f)
  (set-funcs)
  (set-bvops))

(define (symbv-binfunc i)
  (define-symbolic* symfunc (~> (bitvector i) (bitvector i) (bitvector i)))
  symfunc)

(define (make-32bit func)
  (if SYM-FUNC? (symbv-binfunc 32) func))

(define (make-64bit func)
  (if SYM-FUNC? (symbv-binfunc 64) func))

(define (set-funcs)
  (set! smul (make-64bit bvmul))
  (set! sdiv (make-64bit bvudiv))
  (set! smod (make-64bit bvurem))
  (set! smul32 (make-32bit bvmul))
  (set! sdiv32 (make-32bit bvudiv))
  (set! smod32 (make-32bit bvurem)))

(define (set-bvops)
  (set! symbolic-bvops (list smul sdiv smod smul32 sdiv32 smod32)))

(define smul (make-64bit bvmul))
(define sdiv (make-64bit bvudiv))
(define smod (make-64bit bvurem))
(define smul32 (make-32bit bvmul))
(define sdiv32 (make-32bit bvudiv))
(define smod32 (make-32bit bvurem))

(define symbolic-bvops (list smul sdiv smod smul32 sdiv32 smod32))
