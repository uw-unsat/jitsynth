#lang rosette

(require rosette/lib/angelic)

(provide symbv symbv-thunk symbv-func-apply
         bvl bvn0l bv8 bv16 bv32 bv64 zeros
         sextl zextl sext32 zext32 sext64 zext64
         flatten-list sublist? set-equal? check-set-equal?
         bv-size)

#| ############### BITVECTOR UTILS ############### |#
(define (symbv l)
  (define-symbolic* sym (bitvector l))
  sym)

(define (symbv-thunk l)
  (thunk (symbv l)))

(define (symbool)
  (define-symbolic* symtf boolean?)
  symtf)

(define (get-bv-func bitlen thunk-bvop max-depth)
  (if (= 0 max-depth)
    identity
    (let ([inner-func (get-bv-func bitlen thunk-bvop (sub1 max-depth))]
          [act-bvop (thunk-bvop)]
          [use-x-operand? (symbool)])
      (choose*
        identity
        (lambda (x)
          (act-bvop
            (inner-func x)
            (if use-x-operand? x (symbv bitlen))))))))

(define (symbv-func-apply lin lout input)
  (let* ([bvop-list (list bvand bvadd bvshl bvor bvxor bvlshr bvashr)]
         [bvop (thunk (apply choose* bvop-list))]
         [bvfunc-out ((get-bv-func lin bvop 3) input)])
    (cond
      [(= lin lout) bvfunc-out]
      [(< lin lout)
       (define extra (symbv (- lout lin)))
       (choose*
         (concat extra bvfunc-out)
         (concat extra bvfunc-out))]
      [else
        (choose*
          (extract (sub1 lout) 0 bvfunc-out)
          (extract (sub1 lin) (- lin lout) bvfunc-out))])))

(define (bvl l) (lambda (n) (bv n l)))
(define bv8 (bvl 8))
(define bv16 (bvl 16))
(define bv32 (bvl 32))
(define bv64 (bvl 64))

(define (bvn0l l) (bv 0 l))
(define zeros bvn0l)

(define (bv-split vec lens)
  (if (null? lens)
    null
    (cons
      (extract (sub1 (apply + lens)) (apply + (rest lens)) vec)
      (bv-split vec lens))))

(define (sextl l)
  (lambda (x)
    (sign-extend x (bitvector l))))

(define (zextl l)
  (lambda (x)
    (zero-extend x (bitvector l))))

(define sext32 (sextl 32))
(define sext64 (sextl 64))
(define zext32 (zextl 32))
(define zext64 (zextl 64))

#| ############### LIST UTILS ############### |#
(define-syntax (flatten-list stx)
  (syntax-case stx ()
    ((_ exprs ...) #'(flatten (list exprs ...)))))

(define (sublist? small-list big-list)
  (andmap (lambda (elem) (member elem big-list)) small-list))

(define (set-equal? s1 s2)
  (and (sublist? s1 s2) (sublist? s2 s1)))

(require rackunit)
(define-binary-check (check-set-equal? actual expected)
  (set-equal? actual expected))

(define (bv-size val)
  (bitvector-size (type-of val)))
