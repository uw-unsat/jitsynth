#lang rosette

(require "rwsets.rkt"
         "../ams/machine.rkt"
         "../ams/queries.rkt"
         "../common/options.rkt"
         "../common/case-hex.rkt"
         "../common/data-structures.rkt"
         "../common/data-utils.rkt")
(require (only-in "../common/debug.rkt" [debugln common:debugln]))
(require rosette/lib/angelic)

(provide (struct-out sketch-params)
         (struct-out sketch-options)
         fill-cache get-options-set
         make-gensketch-lcs
         clean-cache
         make-compute-func)

; Interface:
; (fill-cache params largest-prog-length)
; (define options-set (get-options-set params))
; (for ([opt options-set])
;   (do-something (make-gensketch-lcs params options)))
; (clean-cache params)

(define DEBUG #f)

(define (debugln x)
  (when DEBUG (common:debugln x)))

(define (debug+ret x)
  (when DEBUG (common:debugln (~a "debug+ret: " x)))
  x)

(struct sketch-params
  (src/tgt-pair src-instr iwsable-src-instr tgt-compute)
  #:transparent)
(struct sketch-options
  (largest-prog-length load store extend param-load-indexes)
  #:transparent)

; NOTE: Full options has null for indexes
;       b/c we don't know the possible indexes yet
(define (full-options-list lpl)
  (list
    (sketch-options lpl #t #t 'sign (hash))
    (sketch-options lpl #t #t 'zero (hash))))

; Cached values
(define tgt-load-params/cache (hash))
(define tgt-load-params/regs (hash))
(define tgt-load-params/asmts (hash))
(define tgt-load-mem #f)
(define tgt-store-mem #f)
(define tgt-load-pc #f)
(define tgt-store-pc #f)
(define tgt-return #f)
(define tgt-reg-ext (hash))
(define tgt-reg-ext/asmts (hash))
(define scratch-regs #f)

; Helper functions
(define (get-param-key param)
  (cons (parameter-name param) (parameter-write-set param)))

(define (take-scratch-reg)
  (if (empty? scratch-regs) #f
    (let ([reg (first scratch-regs)])
      (set! scratch-regs (rest scratch-regs))
      reg)))

(define (tgt-store-state state-ident)
  (case state-ident
    [(pc) (if (unsat? tgt-store-pc) null tgt-store-pc)]
    [(mem) (if (unsat? tgt-store-mem) null tgt-store-mem)]
    [(regs) null]
    [(return?) null]))

(define (tgt-load-state state-ident)
  (case state-ident
    [(pc) (if (unsat? tgt-load-pc) null tgt-load-pc)]
    [(mem) (if (unsat? tgt-load-mem) null tgt-load-mem)]
    [(regs) null]))

(define (reset-caches)
  (set! tgt-load-params/cache (hash))
  (set! tgt-load-params/asmts (hash))
  (set! tgt-load-pc #f)
  (set! tgt-store-pc #f)
  (set! scratch-regs #f))

; Should return a list of valid options
(define (get-options-set sk-params largest-prog-length)
  (define src-instr (sketch-params-src-instr sk-params))
  (define src-read-params
    (filter (lambda (param)
              (and (not (null? (get-param-write-set param)))
                   (is-value? param)))
            (instruction-params src-instr)))
  
  (define (param-load-options
            [remaining-params src-read-params]
            [current-options (list (hash))])
    (cond
      [(empty? remaining-params) current-options]
      [else
        (let* ([param (first remaining-params)]
               [param-key (get-param-key param)]
               [param-loads (hash-ref tgt-load-params/cache param-key)])
        (param-load-options
          (rest remaining-params)
          (flatten-list
            (for*/list ([opt current-options]
                        [param-index (length param-loads)])
              (hash-set opt (parameter-name param) param-index)))))]))

  (define (fits-size? options load-prog store-prog reg-ext)
    (define smallest-prog-length
      ; NOTE: Params should be included in load-prog here
      ; Just call compute length instead?
      (foldl + 0 (map length (list load-prog store-prog reg-ext))))
    (<= smallest-prog-length (sketch-options-largest-prog-length options)))

  (define (compute-length options load-prog store-prog reg-ext)
    (define smallest-prog-length
      ; NOTE: Params should be included in load-prog here
      (foldl + 0 (map length (list load-prog store-prog reg-ext))))
    (- (sketch-options-largest-prog-length options) smallest-prog-length))

  ; If one option is empty, no point in trying it multiple times
  (define (all-nonempty? options load-prog store-prog reg-ext)
    (and
      (or (implies (sketch-options-load options) (not (empty? load-prog)))
          (implies (sketch-options-store options) (not (empty? store-prog))))
      (implies (not (equal? 'none (sketch-options-extend options)))
               (not (empty? reg-ext)))))

  ; NOTE: Algorithm may be sensitive to the returned ordering
  (debugln  (~a "All param options: " (param-load-options)))
  (define opt-compute-length (make-hash))
  (define valid-opts
    (flatten-list
      (for*/list ([ld (if (LS_REORDER?) '(#t #f) '(#f))]
                  [st (if (LS_REORDER?) '(#t #f) '(#f))]
                  [ext (if (LS_REORDER?) '(sign zero none) '(none))]
                  [param-lds (if (LS_REORDER?)
                               (param-load-options)
                               (param-load-options null))]
                  #:when
                  (and (equal? ld st)
                       (not (member -1 (hash-values param-lds)))))
        (let*-values 
          ([(opt) (sketch-options
                    largest-prog-length
                    ld st ext param-lds)]
           ; NOTE: Should return empty load/store 
           ;       when options say #f for load/store
           [(load-prog store-prog lp-asmts)
            (make-load-store-progs sk-params opt)]
           [(reg-ext ext-asmts)
            (make-extends-prog sk-params opt)])
          (debugln (~a "Checking options: " opt))
          (debugln (~a "All nonempty: "
                       (all-nonempty? opt load-prog store-prog reg-ext)))
          (debugln (~a "Fits size: "
                       (fits-size? opt load-prog store-prog reg-ext)))
          (if (and (all-nonempty? opt load-prog store-prog reg-ext)
                   (fits-size? opt load-prog store-prog reg-ext))
            (begin
              (hash-set! opt-compute-length opt
                         (compute-length opt load-prog store-prog reg-ext))
              opt)
            ; NOTE: null will get flattened out
            null)))))

  (define (off-load-pri opt)
    (let ([popts (sketch-options-param-load-indexes opt)])
      (cond
        [(and (hash-has-key? popts 'off)
              (> (hash-ref popts 'off) 0))
         0]
        [else 1])))

  (sort valid-opts
        ; NOTE: This is heuristic
        (lambda (opt1 opt2)
          (let ([size-lt
                  (< (hash-ref opt-compute-length opt1)
                     (hash-ref opt-compute-length opt2))]
                [size-eq
                  (= (hash-ref opt-compute-length opt1)
                     (hash-ref opt-compute-length opt2))]
                [param-lt
                  (< (off-load-pri opt1)
                     (off-load-pri opt2))])
            (if size-eq param-lt size-lt)))))

(define (clean-cache params)
  (clean-cache/si (sketch-params-src-instr params)))

(define (clean-cache/si src-instr)
  (for ([param (instruction-params src-instr)]
        #:when (hash-has-key?
                 tgt-load-params/cache
                 (get-param-key param)))
    (let* ([key (get-param-key param)]
           [val (hash-ref tgt-load-params/cache key)])
      (set! tgt-load-params/cache
        (if (parameter-cachable? param)
          tgt-load-params/cache
          (hash-remove tgt-load-params/cache key)))
      (set! tgt-load-params/asmts
        (if (parameter-cachable? param)
          tgt-load-params/cache
          (hash-remove tgt-load-params/cache key)))
      val)))

; Function gensketch-lcs
; Returns a sketch that loads (l), computes (c), then stores (s)
(define (make-gensketch-lcs params options)
  (define-values (load-prog store-prog ls-asmts)
    (make-load-store-progs params options))
  (define-values (reg-ext ext-asmts)
    (make-extends-prog params options))

  (define ext-option (sketch-options-extend options)) 
 
  (cond
    [(and (empty? reg-ext)
          (not (equal? ext-option 'none)))
     ; NOTE: This case shouldn't happen
     (values (unsat) null 0)]
    [else
      (let-values
        ([(sketch-func sketch-asmts)
          (make-gensketch-lcs-from-ls
            params options
            load-prog store-prog reg-ext
            (flatten-list ls-asmts ext-asmts))])
        (values sketch-func
                sketch-asmts
                (+ (length reg-ext)
                   (length load-prog)
                   (length store-prog) 0)))]))

(define (make-extends-prog params options)
  (define src/tgt-pair
    (sketch-params-src/tgt-pair params))
  (define iwsable-src-instr
    (sketch-params-iwsable-src-instr params))
  (define src-instr
    (sketch-params-src-instr params))
  (define ext-option (sketch-options-extend options)) 
  (define source (get-source src/tgt-pair))
  (define target (get-target src/tgt-pair))

  (debugln "Starting register extends")

  (define param-regs 
    (map (lambda (p) ((parameter-get p) iwsable-src-instr))
         (filter is-reg? (instruction-params src-instr))))

  (define src->tgt-reg (src/tgt-s2t-reg-index src/tgt-pair))

  (debugln (~a "Param regs: " param-regs))

  (define unique-regs
    (map src->tgt-reg (unique-reg-read-set src-instr source)))

  (debugln (~a "Unique regs: " unique-regs))

  (define ext-key (cons ext-option (append param-regs unique-regs)))

  (define-values (reg-ext ext-asmts)
    (cond 
      [(hash-has-key? tgt-reg-ext ext-key)
       (values (hash-ref tgt-reg-ext ext-key)
               (hash-ref tgt-reg-ext/asmts ext-key))]
      [else
        (case ext-option
          [(none) (values null null)]
          [(sign)
           (if (< (get-regs-bitlen source)
                  (get-regs-bitlen target))
             (synth-sign-extend
               source target (append param-regs unique-regs))
             (values null null))]
          [(zero)
           (if (< (get-regs-bitlen source)
                  (get-regs-bitlen target))
             (synth-zero-extend
               source target (append param-regs unique-regs))
             (values null null))]
          [else (error (~a "Invalid register extends option: " ext-option))])]))
  (set! tgt-reg-ext
    (hash-set tgt-reg-ext ext-key reg-ext))
  (set! tgt-reg-ext/asmts
    (hash-set tgt-reg-ext/asmts ext-key ext-asmts))

  (debugln "Register extends complete:")
  (debugln reg-ext)
  
  (values
    (if (unsat? reg-ext) null reg-ext)
    ext-asmts))

(define (fill-cache sk-params largest-prog-length)
  (clear-asserts!)
  (define all-options (full-options-list largest-prog-length))
  (define full-options (first all-options))
  (for ([opt all-options])
    (make-extends-prog sk-params opt))

  (define src/tgt-pair
    (sketch-params-src/tgt-pair sk-params))
  (define iwsable-src-instr
    (sketch-params-iwsable-src-instr sk-params))
  (define src-instr
    (sketch-params-src-instr sk-params))
  (define source (get-source src/tgt-pair))
  (define target (get-target src/tgt-pair))

  (if (not scratch-regs)
    (set! scratch-regs (get-scratch-regs src/tgt-pair))
    (void))

  (debugln "Computing end state")

  (define end-src-state 
    (let ([iws (get-iwss source)]
          [instr iwsable-src-instr]
          [start-state (machine-default-state source)])
      (iws (list instr) start-state (bv 1 64))))
  (clear-asserts!)

  (debugln "End state computed")

  (define src-read-set (get-read-set src-instr))
  (define src-read-params
    (filter (lambda (param)
              (and (not (null? (get-param-write-set param)))
                   (is-value? param)))
            (instruction-params src-instr)))

  (debugln "Source read parameters computed")

  (define pc-scratch-reg
    (if (or (not tgt-store-pc) (not tgt-load-pc))
      (take-scratch-reg)
      #f))

  (if (and pc-scratch-reg (not tgt-store-pc))
    (set! tgt-store-pc (synth-pc-store target pc-scratch-reg))
    (void))

  (debugln "PC store complete")

  (if (and pc-scratch-reg (not tgt-load-pc))
    (set! tgt-load-pc (synth-pc-load target pc-scratch-reg))
    (void))

  (debugln "PC load complete")

  (debugln "Starting parameter load")

  (for ([param src-read-params])
    (when (not (hash-has-key?
                 tgt-load-params/cache
                 (get-param-key param)))
      (let-values
        ([(pload-choices pload-asmts)
          (synth-param-load
            src/tgt-pair
            (if (hash-has-key? tgt-load-params/regs (parameter-name param))
              (hash-ref tgt-load-params/regs (parameter-name param))
              (let ([reg (take-scratch-reg)])
                (set! tgt-load-params/regs
                  (hash-set tgt-load-params/regs
                            (parameter-name param) reg))
                reg))
            ((parameter-get param) iwsable-src-instr)
            largest-prog-length
            (get-source-param-bitlen src/tgt-pair)
            (member 'pc (parameter-write-set param)))])
        (debugln (~a "pload length: " (length pload-choices)))
        (set! tgt-load-params/cache
          (hash-set tgt-load-params/cache
                    (get-param-key param)
                    pload-choices))
        (set! tgt-load-params/asmts
          (hash-set tgt-load-params/asmts
                    (get-param-key param)
                    pload-asmts))
        (debugln (~a "Parameter loads computed for " (parameter-name param)))
        (debugln (~a "Parameter loads: " (hash-ref tgt-load-params/cache
                                                   (get-param-key param)))))))

  (debugln "Parameter loads complete")

  ; TODO fix this query
  ;      Do I want to be precise here?
  ;      Or do I want to just generate the load part of the sketch?
  (set! tgt-store-mem (unsat))
  (debugln "Memory store complete")

  ; TODO fix this query
  (set! tgt-load-mem (unsat))
  (debugln "Memory load complete")

  ; TODO fix this query (or remove)
  (set! tgt-return (unsat))
  (debugln "Return complete")

  (debugln "Done filling cache"))

(define (make-load-store-progs sk-params sk-options)
  (define src/tgt-pair (sketch-params-src/tgt-pair sk-params))
  (define iwsable-src-instr (sketch-params-iwsable-src-instr sk-params))
  (define src-instr (sketch-params-src-instr sk-params))
  (define source (get-source src/tgt-pair))
  (define target (get-target src/tgt-pair))
  (define use-load? (sketch-options-load sk-options))
  (define use-store? (sketch-options-store sk-options))

  ; NOTE: Registers are implicitly "stored"
  (define store-values
    (let ([ws (instruction-write-set src-instr)])
      (flatten-list
        (map (lambda (p) (if (unsat? p) null p))
             (for/list ([state-ident ws])
               (debugln (~a "Store state: " state-ident))
               (debugln (~a "Value: " (tgt-store-state state-ident)))
               (tgt-store-state state-ident))))))

  (define (tgt-load-param param)
    (cond
      [(hash-has-key?
         (sketch-options-param-load-indexes sk-options)
         (parameter-name param))
       (let ([key (get-param-key param)]
             [index
               (hash-ref
                 (sketch-options-param-load-indexes sk-options)
                 (parameter-name param))])
         (list-ref (hash-ref tgt-load-params/cache key) index))]
      [else null]))

  (define (tgt-param-asmts param)
    (cond
      [(hash-has-key?
         (sketch-options-param-load-indexes sk-options)
         (parameter-name param))
       (let ([key (get-param-key param)]
             [index
               (hash-ref
                 (sketch-options-param-load-indexes sk-options)
                 (parameter-name param))])
         (list-ref (hash-ref tgt-load-params/asmts key) index))]
      [else null]))

  (define src-read-set (get-read-set src-instr))
  (define src-read-params
    (filter (lambda (param)
              (and (not (null? (get-param-write-set param)))
                   (is-value? param)))
            (instruction-params src-instr)))

  (define param-asmts
    (flatten-list
      (for/list ([param src-read-params])
        (tgt-param-asmts param))))

  (define load-values
    (let ([rs src-read-set]
          [prs src-read-params])
      (debugln (~a "Prs: " (map parameter-name prs)))
      (debugln (~a "Param values: "
                   (for/list ([f (map parameter-get prs)])
                     (f iwsable-src-instr))))
      (flatten-list
        (map (lambda (p) (if (unsat? p) null p))
             (append
               (for/list ([state-ident rs])
                 (tgt-load-state state-ident))
               (for/list ([param prs])
                 (tgt-load-param param)))))))

  (debugln (~a "For options " sk-options))
  (debugln (~a "Load: " load-values))
  (debugln (~a "Store: " store-values))
  (values
    (if use-load? load-values null)
    (if use-store? store-values null)
    (if use-load? param-asmts null)))

(define (get-value-mapping src-instr iwsable-src-instr tgt-param)
  ; NOTE: This if statement should contain only concrete conditions
  (flatten-list
    (for/list ([src-param (instruction-params src-instr)])
      (let* ([src-bitlen (get-param-bitlen src-param)]
             [tgt-bitlen (get-param-bitlen tgt-param)]
             [condition
               (and (<= 0 src-bitlen)
                    (<= 0 tgt-bitlen)
                    (<= src-bitlen tgt-bitlen)
                    (is-value? src-param)
                    (is-value? tgt-param)
                    ; if the write set is incorrect
                    (not (empty? (get-param-write-set tgt-param)))
                    (if (LS_REORDER?)
                      (sublist?
                        (get-param-write-set tgt-param)
                        (get-param-write-set src-param))
                      #t))])
        (if condition
          (for/list ([f (list zero-extend sign-extend)]
                     [p (make-list 2 ((parameter-get src-param) iwsable-src-instr))])
            (f p (bitvector tgt-bitlen)))
          null)))))

; NOTE: For now, assuming commutative bops (+/*)
(define (augment-value-mapping choices bops consts max-depth)
  (if (or (empty? choices) (<= max-depth 0))
    choices
    (augment-value-mapping
      (flatten-list
        choices
        (for*/list ([choice choices]
                    [op bops]
                    [c consts])
          (op (bv c (bv-size choice)) choice)))
      bops consts (sub1 max-depth))))

(define (get-reg-mapping sk-params tgt-param)
  (define src/tgt-pair (sketch-params-src/tgt-pair sk-params))
  (define iwsable-src-instr (sketch-params-iwsable-src-instr sk-params))
  (define src-instr (sketch-params-src-instr sk-params))
  (define target (src/tgt-target src/tgt-pair))

  (define (src->tgt-reg index)
    (extract (sub1 (get-regbits target)) 0
             ((src/tgt-s2t-reg-index src/tgt-pair)
              (zero-extend index (bitvector 8)))))

  (define src-instr-regs
    (map
      (lambda (p)
        ((parameter-get p) iwsable-src-instr))
      (filter is-reg? (instruction-params src-instr))))

  (if (is-reg? tgt-param)
    (map src->tgt-reg src-instr-regs)
    null))

(define (get-choice-no-param-corr
          src-instr iwsable-src-instr
          pv param-bits)
  (if (positive? param-bits)
    (symbv-func-apply
      (get-instr-bitlen src-instr)
      param-bits
      (get-instr-as-bv src-instr iwsable-src-instr))
    pv))

(define (make-compute-func
          sk-params options
          load-prog store-prog ext-prog)
  (define src/tgt-pair (sketch-params-src/tgt-pair sk-params))
  (define iwsable-src-instr (sketch-params-iwsable-src-instr sk-params))
  (define src-instr (sketch-params-src-instr sk-params))
  (define tgt-compute (sketch-params-tgt-compute sk-params))
  (define source (get-source src/tgt-pair))
  (define target (get-target src/tgt-pair))

  (define sym-instrs-func
    (let* ([rs (get-read-set src-instr)]
           [ws (get-write-set src-instr)]
           [instrs (get-instrs-for-ws target (flatten-list ws 'pc))])
      (lambda (proglen)
        (if (empty? instrs)
          (error "Unable to generate sketch because no instructions match read/write sets: "
                 rs ", " ws)
          (apply choose*
            (for/list ([tgt-instr-choice instrs])
              (let* ([tgt-instr-name (instruction-name tgt-instr-choice)]
                     [tgt-instr (get-instr target tgt-instr-name)]
                     [instr-params (instruction-params (get-instr target tgt-instr-name))]
                     [instr-make (instruction-make tgt-instr)])
                (apply instr-make
                  (for/list ([param instr-params])
                    (for*/all ([act-param param]
                               [pv ((parameter-value-func act-param))])
                      (if (PARAM_CORR?)
                        (apply choose*
                          (cons
                            ; NOTE: pv may be a list, so can't put in flatten-list
                            pv
                            (flatten-list
                              (get-reg-mapping sk-params param)
                              (let* ([mapping (get-value-mapping
                                                src-instr iwsable-src-instr param)]
                                     [aug-map (augment-value-mapping
                                                mapping
                                                ; NOTE: 3 lines below are heuristic
                                                (list bvmul bvadd)
                                                (list 1 -1 proglen)
                                                2)])
                                aug-map))))
                        (apply choose*
                          (cons
                            (get-choice-no-param-corr
                              src-instr iwsable-src-instr
                              pv (parameter-bitlen act-param))
                            (get-reg-mapping sk-params param))))))))))))))

  ; NOTE: Not yet supporting calls/returns/exits
  (define (compute-sketch compute-length)
    (if tgt-compute
      (if (equal? (length tgt-compute) compute-length)
        tgt-compute
        null)
      (build-list
        compute-length
        ; computation
        (thunk*
          (sym-instrs-func
            (+ compute-length
               (length ext-prog)
               (length load-prog)
               (length store-prog)))))))

  compute-sketch)

(define (make-gensketch-lcs-from-ls
          params options
          load-prog store-prog ext-prog asmts)
  (define compute-sketch
    (make-compute-func
      params options
      load-prog store-prog ext-prog))

  (make-gensketch-lcs-from-ls-with-compute
    params options
    load-prog store-prog ext-prog
    compute-sketch asmts))

(define (make-gensketch-lcs-from-ls-with-compute
          params options
          load-prog store-prog ext-prog
          computation ; Function of sketch length
          asmts)
          ; NOTE: Value of -1 indicates that there is no largest program length
  ; (debugln (~a (length load-prog) " " (length store-prog) " " largest-prog-length))
  ; (assert (<= (+ (length load-prog) (length store-prog)) largest-prog-length))
  (define src/tgt-pair (sketch-params-src/tgt-pair params))
  (define iwsable-src-instr (sketch-params-iwsable-src-instr params))
  (define src-instr (sketch-params-src-instr params))
  (define largest-prog-length (sketch-options-largest-prog-length options))

  (define (gensketch-lcs compute-length)
    (debugln "Making gensketch")
    (define lp load-prog)
    (define sp store-prog)
    (define res
      (append lp ext-prog (computation compute-length) sp
              (if (equal? largest-prog-length -1)
                empty
                (make-list
                  (- largest-prog-length
                     (length lp) (length sp) (length ext-prog) compute-length)
                  (machine-nopi (src/tgt-target src/tgt-pair))))))
    (debugln "Done making gensketch")
    res)

  (debugln "Gensketch defined")

  (values gensketch-lcs asmts))
