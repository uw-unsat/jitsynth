#lang rosette

(require "query-utils.rkt"
         "../common/symbolic-ops.rkt"
         "../gensketch/gensketch.rkt"
         "../ams/machine.rkt"
         "../genc/genc.rkt"
         "../common/data-utils.rkt"
         "../common/file-utils.rkt"
         "../common/time-utils.rkt"
         "../common/options.rkt"
         "../common/data-structures.rkt")
(require (only-in "../common/debug.rkt" [debugln common:debugln]))

; TODO remove
(require "../common/instructions.rkt")

(require rosette/lib/angelic)
(require file/glob)
(require racket/engine)

(provide check-partial-compiler synth-partial-compiler
         synth-compiler synth-compiler-parallel
         synth-compiler-parallel-with-timeout
         synth-compiler-instr-parallel)

(define DEBUG #f)

(define (debugln x [dbg DEBUG])
  (when dbg (common:debugln x)))

(define (get-assume-func
          src/tgt-pair src:iws canon-src-instr
          tgt-start-state src-start-state
          other-asserts)
  (define pc-fact (src/tgt-pc-fact src/tgt-pair))

  (lambda (sketch)
    (for ([asmt other-asserts])
      (assert asmt))

    ; TODO remove
    ; (assert (bveq (bv64 0) (state-pc tgt-start-state)))
    ; (assert (bveq (old-bpf-instr-imm canon-src-instr) (bv #x80000000 32)))
    ; (assert (bveq (old-bpf-instr-toff canon-src-instr) (bv 1 8)))
    ; (assert (bveq (old-bpf-instr-foff canon-src-instr) (bv 3 8)))

    ; TODO have options for these invariants
    ; (assert (bveq (read-register (state-regs tgt-start-state) (bv 0 8))
    ;               (sign-extend (extract 31 0 (read-register (state-regs tgt-start-state) (bv 0 8))) (bitvector 64))))

    ; TODO should this be the source start state?
    ; TODO don't hardcode the x4 (tho idk how to get it in there, maybe use some conversion thing)
    (assert (bveq (bv64 0) (bvurem (state-pc tgt-start-state) (bvmul (bv64 pc-fact) (bv64 (length sketch))))))
    (src:iws
      (list canon-src-instr)
      (make-state
        (state-regs src-start-state)
        (state-mem src-start-state)
        ; TODO is this too inefficient?
        (bvudiv (state-pc src-start-state) (bv (length sketch) 64)))
      (bv64 1))))

(define (end-nops am instrs)
  (if (or (empty? instrs)
          (not (empty? (symbolics (last instrs))))
          (not (equal? (last instrs) (machine-nopi am))))
    0
    (add1 (end-nops am (take instrs (sub1 (length instrs)))))))

(define (get-guarantee-func
          src/tgt-pair tgt:iws src:iws
          canon-src-instr canon-src-asmts
          tgt-start-state src-start-state)
  (define source (get-source src/tgt-pair))
  (define target (get-target src/tgt-pair))
  (define pc-fact (src/tgt-pc-fact src/tgt-pair))

  (lambda (sketch)
    (let* ([src-state
             (src:iws
               (list canon-src-instr)
               (make-state
                 (state-regs src-start-state)
                 (state-mem src-start-state)
                 (bvudiv (state-pc src-start-state) (bv (length sketch) 64)))
               (bv64 1))]
           [tgt-end-nops (end-nops target sketch)]
           [tgt-bvlen-w/o-nops (bv64 (- (length sketch) tgt-end-nops))]
           [tgt-state (tgt:iws sketch tgt-start-state tgt-bvlen-w/o-nops)])
      (apply-state-invars source src-state)
      (apply-state-invars target tgt-state)
      ; TODO have options for these invariants
      ; (assert (bveq (read-register (state-regs tgt-state) (bv 0 8))
      ;               (sign-extend (extract 31 0 (read-register (state-regs tgt-state) (bv 0 8))) (bitvector 64))))
      ; (debugln (~a "Tgt state return?: " (state-return? tgt-state)))
      ; (assert (bveq (read-register (state-regs tgt-state) (bv 0 8))
      ;               (read-register (state-regs src-state) (bv 0 8))))
      (assert
        (state-regs-equal?
          source
          ((src/tgt-t2s-regs src/tgt-pair) (state-regs tgt-state))
          (state-regs src-state)))
      (assert
        (state-mem-equal?
          source
          (state-mem tgt-state)
          (state-mem src-state)))
      (if (empty? sketch)
        (assert (bveq (state-pc src-state) (state-pc src-start-state)))
        (assert
          (state-pc-equal?
            ((src/tgt-t2s-pc src/tgt-pair)
             (if (bveq (state-pc tgt-state)
                       (bvadd (state-pc tgt-start-state)
                              (bvmul (bv64 pc-fact) tgt-bvlen-w/o-nops)))
               (bvadd (state-pc tgt-state)
                      (bvmul (bv64 pc-fact) (bv64 tgt-end-nops)))
               (state-pc tgt-state)))
            (bvmul (bv64 (length sketch)) (state-pc src-state)))))
      (when USE_RETURN?
        (assert (equal? (state-return? src-state) (state-return? tgt-state))))
    )))

; TODO this has been behind for a while
(define (check-partial-compiler
          src/tgt-pair src-instr tgt-compute
          #:lpl [largest-prog-len MAXPROGLEN])
  (clear-asserts!)
  (set-sym)
  (let*-values
    ([(source) (get-source src/tgt-pair)]
     [(target) (get-target src/tgt-pair)]
     [(tgt:iws) (get-iwss target)]
     [(src:iws) (get-iwss source)]
     [(canon-src-instr canon-src-asmts) (with-asserts (make-canon-instr source src-instr))]
     [(tgt-start-state) (machine-default-state target)]
     [(src-start-state) (t2s-state src/tgt-pair tgt-start-state)]
     [(src-state-asmts)
      (with-asserts-only (apply-state-asmts source src-start-state))]
     [(tgt-state-asmts)
      (with-asserts-only (apply-state-asmts target tgt-start-state))]
     [(src-state-invars)
      (with-asserts-only (apply-state-invars source src-start-state))]
     [(tgt-state-invars)
      (with-asserts-only (apply-state-invars target tgt-start-state))]
     [(sketch-func sketch-asmts comp-max-compute-len)
      (make-gensketch-lcs
        src/tgt-pair
        src-instr
        canon-src-instr
        largest-prog-len
        #t #t
        #:compute tgt-compute)]
     [(no-comp-sketch-func no-comp-sketch-asmts no-comp-max-compute-len)
      (make-gensketch-lcs
        src/tgt-pair
        src-instr
        canon-src-instr
        largest-prog-len
        #t #t)]
     [(assume-func)
      (get-assume-func
        src/tgt-pair
        src:iws
        canon-src-instr 
        tgt-start-state
        src-start-state
        (flatten-list
          src-state-asmts
          tgt-state-asmts
          src-state-invars
          tgt-state-invars
          canon-src-asmts))]
     [(guarantee-func)
      (get-guarantee-func
        src/tgt-pair tgt:iws src:iws
        canon-src-instr canon-src-asmts
        tgt-start-state src-start-state)])
    ; NOTE: This should be empty
    (debugln (~a "Missed asserts: " (asserts)))
    (define-values (prog prog-asmts)
      (with-asserts (sketch-func (length tgt-compute))))
    (define-values (compute-sketch compute-asmts)
      (with-asserts
        ((make-compute-func
           src/tgt-pair
           src-instr
           canon-src-instr)
         (length tgt-compute))))
    ; TODO finish this
    (define-values (no-comp-sketch no-comp-asmts)
      (with-asserts (no-comp-sketch-func (length tgt-compute))))

    (debugln "Start verifying")

    (define verify-mdl
      (verify #:assume
              (begin
                (for ([asmt prog-asmts])
                  (assert asmt))
                (assume-func prog))
              #:guarantee (guarantee-func prog)))

    (debugln "Done verifying")

    (define solve-mdl
      (synthesize
        #:forall
        (list canon-src-instr
              symbolic-bvops
              tgt-start-state
              (get-equality-indicies source))
        #:assume
        (begin
          (for ([asmt prog-asmts])
            (assert asmt))
          (for ([asmt compute-asmts])
            (assert asmt))
          (assume-func prog)
          (assume-func compute-sketch))
        #:guarantee
        (for ([tgt-instr tgt-compute]
              [sketch-instr compute-sketch])
          (assert (equal? tgt-instr sketch-instr)))))

    (debugln "Done solving")

    (define synth-mdl (unsat))
    #| TODO make option
      (synthesize
        #:forall
        (list canon-src-instr
              symbolic-bvops
              tgt-start-state
              (get-equality-indicies source))
        #:assume
        (begin
          (for ([asmt no-comp-asmts])
            (assert asmt))
          (assume-func no-comp-sketch))
        #:guarantee
        (guarantee-func no-comp-sketch)))
  |#

    (debugln "Done synthesizing")

    (if (not (unsat? verify-mdl))
      (debugln "Given target compute is not correct.")
      (void))

    (if (unsat? solve-mdl)
      (debugln "Given target compute is not in the synthesis sketch.")
      (void))

    (if (unsat? synth-mdl)
      (debugln "Unable to synthesize target program for source instruction.")
      (void))

    (debugln (~a "Verify Model: " verify-mdl))
    ; (debugln (~a "Solve Model: " solve-mdl))
    ; (debugln (~a "Synth Model: " synth-mdl))

    ; TODO (BIG): May want to only have one return statement
    ; (to avoid this extra return? value in state)
    ; May be difficult to justify the return thing (meaning not so general)

    (debugln (~a "Canon src instr: " canon-src-instr))
    (if (not (unsat? verify-mdl))
      (let*
        ([res-src-state
           (evaluate
             (src:iws
               (list canon-src-instr)
               (make-state
                 (state-regs src-start-state)
                 (state-mem src-start-state)
                 ; TODO is this too inefficient?
                 (bvudiv (state-pc src-start-state) (bv (length prog) 64)))
               (bv64 1))
             verify-mdl)]
         [dbg (debugln "Done with res src state")]
         [res-tgt-state
           (evaluate
             (tgt:iws
               prog
               tgt-start-state
               (bv64 (length prog)))
             verify-mdl)]
         [dbg (debugln "Done with res tgt state")])
        (debug-registers (state-regs src-start-state) DEBUG verify-mdl)
        (debug-registers (state-regs res-src-state) DEBUG verify-mdl)
        (debug-registers (state-regs res-tgt-state) DEBUG verify-mdl)
        (debugln (~a "Src PC: " (state-pc res-src-state)))
        (debugln (~a "Tgt PC: " (state-pc res-tgt-state))))
        #|
        (debugln
          (evaluate
            (state-regs-equal?
              source
              ((src/tgt-t2s-regs src/tgt-pair) (state-regs res-src-state))
              (state-regs res-tgt-state))
            verify-mdl))
        |#
      (void))


    (and
      (unsat? verify-mdl)
      (not (unsat? solve-mdl)))))

(define (synth-partial-compiler sk-params options canon-src-asmts)
  (define src-instr (sketch-params-src-instr sk-params))
  (define src/tgt-pair (sketch-params-src/tgt-pair sk-params))
  (define largest-prog-len (sketch-options-largest-prog-length options))
  (define source (src/tgt-source src/tgt-pair))
  (define canon-src-instr (sketch-params-iwsable-src-instr sk-params))

  ; TODO Do this the better way
  (set! sk-params
    (sketch-params
      (sketch-params-src/tgt-pair sk-params)
      (sketch-params-src-instr sk-params)
      canon-src-instr
      (sketch-params-tgt-compute sk-params)))

  (debugln (~a "Starting instruction  " (instruction-name src-instr)))
  (debugln (~a "Options: " options))
  (clear-asserts!)
  (set-sym)
  ; TODO want some type of clear-asserts!, but currently asserting riscv-pc is below some bound
  ;      do this by encapsulating the assumptions generated below
  ; TODO use with-asserts on most of this stuff
  (let*-values
    ([(source) (get-source src/tgt-pair)]
     [(target) (get-target src/tgt-pair)]
     [(tgt:iws) (get-iwss target)]
     [(src:iws) (get-iwss source)]
     [(tgt-start-state) (machine-default-state target)]
     [(src-start-state) (t2s-state src/tgt-pair tgt-start-state)]
     [(src-state-asmts)
      (with-asserts-only (apply-state-asmts source src-start-state))]
     [(tgt-state-asmts)
      (with-asserts-only (apply-state-asmts target tgt-start-state))]
     [(src-state-invars)
      (with-asserts-only (apply-state-invars source src-start-state))]
     [(tgt-state-invars)
      (with-asserts-only (apply-state-invars target tgt-start-state))]
     [(sketch-func sketch-asmts ls-len)
      (make-gensketch-lcs sk-params options)]
     [(assume-func)
      (get-assume-func
        src/tgt-pair
        src:iws
        canon-src-instr
        tgt-start-state
        src-start-state
        (flatten-list
          canon-src-asmts
          src-state-asmts
          tgt-state-asmts
          src-state-invars
          tgt-state-invars
          sketch-asmts))]
     [(guarantee-func)
      (get-guarantee-func
        src/tgt-pair tgt:iws src:iws
        canon-src-instr canon-src-asmts
        tgt-start-state src-start-state)])
    ; NOTE: This should be empty
    (debugln (~a "Missed asserts: " (asserts)))
    (if (unsat? sketch-func)
      (unsat)
      (let*-values
        ([(lpl-1?) (equal? largest-prog-len -1)]
         [(res mdl _)
          (if (or (< ls-len 0)
                  (> ls-len (if lpl-1? MAXPROGLEN largest-prog-len)))
            (values (unsat) (unsat) #f)
            (synth-from-gensketch
              sketch-func
              (list canon-src-instr
                    symbolic-bvops
                    tgt-start-state
                    (get-equality-indicies source))
              guarantee-func
              (if lpl-1? -1 (- largest-prog-len ls-len))
              #:assume-func assume-func
              #:with-model? #t))])
        (debugln "Result:")
        (debugln res)
        res))))

(define-syntax not-unsat-or
  (syntax-rules ()
    [(not-unsat-or a) a]
    [(not-unsat-or a b ...)
     (if (unsat? a)
       (not-unsat-or b ...)
       a)]))

; TODO better naming convention for returning list vs prog vs hash
; Here, I'm returning a list
(define (synth-compiler-const-lpl src/tgt-pair instrs largest-prog-len)
  (cond
    [(empty? instrs) null]
    [else
      (define-values
        (canon-src-instr canon-src-asmts)
        (with-asserts (make-canon-instr (src/tgt-source src/tgt-pair) (first instrs))))
      ; NOTE: tgt-compute is #f (not pre-decided)
      (define params (sketch-params src/tgt-pair (first instrs) canon-src-instr #f))
      (fill-cache params largest-prog-len)
      (define options-set (get-options-set params largest-prog-len))

      (debugln (~a "Options set: " options-set))

      (define (spc options)
        (synth-partial-compiler params options canon-src-asmts))

      (define (get-my-prog [remaining-opts options-set])
        (cond
          [(empty? remaining-opts) (unsat)]
          [else
            (define res (spc (first remaining-opts)))
            (if (unsat? res)
              (get-my-prog (rest remaining-opts))
              res)]))
      (define my-prog (get-my-prog))

      ; NOTE: iwsable-src-instr not needed for clean-cache
      (clean-cache params)

      (cond
        [(unsat? my-prog) (unsat)]
        [else
          (define rest-compiler
            (synth-compiler-const-lpl
              src/tgt-pair (rest instrs) largest-prog-len))
          (cond
            [(unsat? rest-compiler) (unsat)]
            [else (cons my-prog rest-compiler)])])]))

(define (synth-compiler-iter-lpl src/tgt-pair instrs [lpl MINPROGLEN])
  (debugln (~a "New iter at lpl: " lpl))
  (if (> lpl MAXPROGLEN)
    (unsat)
    (let ([sccl (synth-compiler-const-lpl src/tgt-pair instrs lpl)])
      (if (unsat? sccl)
        (synth-compiler-iter-lpl src/tgt-pair instrs (add1 lpl))
        sccl))))

; NOTE: Not actually unbound: still adheres to MAXPROGLEN
(define (synth-compiler-unbound-lpl src/tgt-pair instrs)
  (if (empty? instrs) empty
    (let ([instr-comp
            (synth-compiler-iter-lpl src/tgt-pair (list (first instrs)))])
      (if (unsat? instr-comp)
        (unsat)
        (let ([rest-comp
                (synth-compiler-unbound-lpl src/tgt-pair (rest instrs))])
          (if (unsat? rest-comp)
            (unsat)
            (cons (first instr-comp) rest-comp)))))))

; NOTE: Currently assuming all lists here are non-empty
;       Want to check these cases later
; Returning a mutable hash here
(define (synth-compiler src/tgt-pair)
  (clear-asserts!)
  (set-sym)
  (define source (get-source src/tgt-pair))
  (define target (get-target src/tgt-pair))
  (define pc-instrs (get-instrs-for-strict-ws source '(pc)))
  (define other-instrs (get-instrs-compl source pc-instrs))
  (debugln (~a "PC instrs: " (map instruction-name pc-instrs)))
  (debugln (~a "Otther instrs: " (map instruction-name other-instrs)))
  (define o-comp (synth-compiler-unbound-lpl src/tgt-pair other-instrs))
  (if (unsat? o-comp)
    (unsat)
    (let ([pc-comp (synth-compiler-iter-lpl
                     src/tgt-pair pc-instrs
                     (apply max (cons MINPROGLEN (map length o-comp))))])
      (if (unsat? pc-comp)
        (unsat)
        (let ([lpl (apply max (map length (append o-comp pc-comp)))])
          (make-hash
            (append
              (for/list ([o-instr other-instrs]
                         [o-tgtprog o-comp])
                (cons
                  (instruction-name o-instr)
                  (append o-tgtprog
                          (make-list (- lpl (length o-tgtprog))
                                     (machine-nopi target)))))
              (for/list ([pc-instr pc-instrs]
                         [pc-tgtprog pc-comp])
                (cons
                  (instruction-name pc-instr)
                  pc-tgtprog)))))))))

(define (synth-compiler-atom
          stp-file src-instr-name largest-prog-length
          #:cache-dir [cache-dir "cchh"])
  ; NOTE: Assuming stp-file contains function "make-stp"
  ;       That takes optional arguments src-instrs and tgt-instrs
  (debugln (~a "Atom " largest-prog-length) #t)
  (cond
    ; Check to see if this has already been computed (possibly by previous run)
    [(cch-file-exists
       stp-file src-instr-name largest-prog-length
       #:cache-dir cache-dir)
     (debugln (~a "Skipping atom " largest-prog-length " because I already synthesized it") #t)]
    [else
      (define make-stp (dynamic-require stp-file 'make-stp))
      (define stp (make-stp (list src-instr-name)))
      (define source (src/tgt-source stp))
      (define target (src/tgt-target stp))
      (define mch-instr (get-instr source src-instr-name))
      (define canon-instr (make-canon-instr source mch-instr))
      ; This returns a singlton list, or unsat
      (define tgt-progs (synth-compiler-const-lpl stp (list mch-instr) largest-prog-length))
      ; TODO do I want to output to file here?
      (debugln (~a "Done with synth for atom " largest-prog-length
                   ": " (if (unsat? tgt-progs)
                          "failed"
                          "success!"))
               #t)
      (write-to-dir
        (~a (stp-file->dir stp-file #:cache-dir cache-dir) "/" src-instr-name)
        (~a largest-prog-length ".cch")
        (if (unsat? tgt-progs)
          'unsat
          (tgt-prog->cfunc canon-instr (first tgt-progs) mch-instr target)))
      (debugln (~a "Done writing for atom " largest-prog-length) #t)]))

; Place list global so that I can kill stuff
(define place-list null)
(define (reset-place-list)
  (for ([pl place-list]) (place-kill pl))
  (set! place-list null))

(define (synth-compiler-instr-parallel
          stp-file src-instr-name
          [min-prog-length MINPROGLEN]
          #:cache-dir [cache-dir "cchh"])
  (debugln (~a "Starting synthesis for instruction " src-instr-name) #t)

  (define curr-opts (list (LS_REORDER?) (PARAM_CORR?) (RW_PRUNE?) (RWSETS?)))
  (define-values (pin pout) (place-channel))
  (define (start-comp len)
    (let ([pl (place/context ch
                (apply
                  set-optimization-options
                  curr-opts)
                (time-and-output
                  (synth-compiler-atom
                    stp-file src-instr-name len
                    #:cache-dir cache-dir)
                  (~a src-instr-name "," len ","))
                (place-channel-put (place-channel-get ch) len))])
      (place-channel-put pl pin)
      pl))

  ; TODO remove
  (debugln min-prog-length #t)
  (define poss-tries (add1 (- MAXPROGLEN min-prog-length)))
  (define next-len (+ min-prog-length (min poss-tries THREADS)))

  (set! place-list
    (for/list ([len (range min-prog-length next-len)])
      (when (not (= len min-prog-length))
        ; Give smaller programs a slight advantage
        (sleep SLEEPTIME))
      (start-comp len)))

  ; Get the first length that finished
  (define res
    (let get-res ([i 0])
      (cond
        [(> i poss-tries) null]
        [else
          (define finished (place-channel-get pout))
          ; Wait for a little bit and get the smallest program
          ; TODO should I wait for longer?
          (sleep SLEEPTIME)
          (define finished-res
            (read-from-dir
              (~a (stp-file->dir stp-file #:cache-dir cache-dir) "/" src-instr-name)
              (~a finished ".cch")))
          (cond
            [(equal? 'unsat finished-res)
             ; Only start next process when we meet the bounds
             (when (<= next-len MAXPROGLEN)
               (set! next-len (add1 next-len))
               (set! place-list
                 (cons
                   ; Start new thread with next length
                   (start-comp (sub1 next-len))
                   place-list)))
             (get-res (add1 i))]
            [else finished])])))

  (for ([pl place-list]) (place-kill pl))
  res)

(define (found-solution?
          stp-file src-instr-name
          largest-prog-length
          #:cache-dir cache-dir)
  (and (cch-file-exists
         stp-file src-instr-name largest-prog-length
         #:cache-dir cache-dir)
       (let ([cfunc-prog
               (read-from-dir
                 (~a (stp-file->dir stp-file #:cache-dir cache-dir) "/" src-instr-name)
                 (~a largest-prog-length ".cch"))])
         (not (equal? 'unsat cfunc-prog)))))

(define (smallest-prog-length
          stp-file src-instr-name
          [lower-bound 0]
          #:cache-dir cache-dir)
  (define dir (~a (stp-file->dir stp-file #:cache-dir cache-dir)
                  "/" src-instr-name))
  (define file-names
    (map (lambda (file)
           (last (string-split (path->string file) "/")))
         (glob (~a dir "/*.cch"))))
  (define results
    (filter
      (lambda (res) (not (equal? 'unsat res)))
      (map (lambda (file)
             (read-from-dir dir file))
           file-names)))
  (define result-lens
    (filter (lambda (l) (>= l lower-bound)) (map length results)))
  (cond
    [(empty? results) #f]
    [(empty? result-lens) 0]
    [else (apply min result-lens)]))

(define (max-smallest-prog
          stp-file instrs
          [prev-msp 0]
          #:cache-dir cache-dir)
  (let* ([smallest-progs
           (map
             (lambda (instr)
               (smallest-prog-length
                 stp-file
                 (instruction-name instr)
                 prev-msp
                 #:cache-dir cache-dir))
             instrs)]
         [msp
           (if (member #f smallest-progs)
             #f
             (apply max smallest-progs))])
    (cond
      [(not msp) msp]
      [(equal? msp prev-msp) msp]
      [else (max-smallest-prog stp-file instrs msp #:cache-dir cache-dir)])))

(define (pc-instrs-consistent?
          stp-file pc-instrs
          #:cache-dir cache-dir)
  (define names (map instruction-name pc-instrs))
  (define req-length
    (max-smallest-prog
      stp-file pc-instrs
      #:cache-dir cache-dir))
  (for/and ([name names])
    (found-solution?
      stp-file name req-length
      #:cache-dir cache-dir)))

(define (synth-compiler-parallel
          stp-file
          [optimization-options '(#t #t #t #t)]
          #:cache-dir [cache-dir "cchh"])
  ; Set optimization options
  (cond
    [(= 4 (length optimization-options))
     (apply
       set-optimization-options
       optimization-options)]
    [else
      (displayln "Wrong number of optimization options. Setting to default.")
     (set-optimization-options #t #t #t #t)])

  ; Fill cache
  (define stp ((dynamic-require stp-file 'make-stp)))
  (define source (src/tgt-source stp))
  (define pc-instrs (get-instrs-for-strict-ws source '(pc)))
  (define other-instrs (get-instrs-compl source pc-instrs))

  ; TODO need to also synthesize cfunc for emitting a nop or smth
  (for ([instr other-instrs])
    (time-and-output
      (synth-compiler-instr-parallel
      stp-file (instruction-name instr)
      #:cache-dir cache-dir)
      (~a (instruction-name instr)
          ",all,")))

  (define min-prog-length
    (if (empty? other-instrs)
      MINPROGLEN
      (max-smallest-prog
        stp-file other-instrs
        #:cache-dir cache-dir)))

  (define (pc-instr-synth mpl [instrs pc-instrs])
    (when (not (empty? instrs))
      (time-and-output
        (synth-compiler-instr-parallel
          stp-file (instruction-name (first instrs)) mpl
          #:cache-dir cache-dir)
        (~a (instruction-name (first instrs))
            "," mpl ",all,"))
      (let ([spl (smallest-prog-length
                   stp-file
                   (instruction-name (first instrs))
                   #:cache-dir cache-dir)])
        (if spl
          (pc-instr-synth
            (max mpl spl)
            (rest instrs))
          ; Return #f when synthesis fails
          #f))))

  (let rec-pc-instr-synth ()
    (when (and
            (not (empty? pc-instrs))
            (not (pc-instrs-consistent?
                   stp-file pc-instrs
                   #:cache-dir cache-dir)))
      (let* ([max-pc-mpl
               (max-smallest-prog
                 stp-file pc-instrs
                 #:cache-dir cache-dir)]
             [mpl (if max-pc-mpl max-pc-mpl min-prog-length)])
        ; When synthesis hasn't failed
        (when (pc-instr-synth mpl)
          (rec-pc-instr-synth)))))

  (define all-prog-length
    (max-smallest-prog
      stp-file (append pc-instrs other-instrs)
      #:cache-dir cache-dir))

  ; TODO should be able to do this without any synthesis
  ;      but this is fine for now
  (for ([instr other-instrs])
    (synth-compiler-atom
      stp-file
      (instruction-name instr)
      all-prog-length
      #:cache-dir cache-dir))

  (write-to-dir
    (stp-file->dir stp-file #:cache-dir cache-dir)
    "proglen.cch"
    all-prog-length))

(define (synth-compiler-parallel-with-timeout
          stp-file
          [optimization-options '(#t #t #t)]
          #:cache-dir [cache-dir "cchh"])
  (if (equal? -1 OVERALL_TIMEOUT)
    (synth-compiler-parallel
      stp-file optimization-options
      #:cache-dir cache-dir)
    (let* ([func
             (thunk*
               (synth-compiler-parallel
                 stp-file optimization-options
                 #:cache-dir cache-dir))]
           [eng (engine func)]
           [res? (engine-run OVERALL_TIMEOUT eng)])
      (when (not res?)
        (debugln "Timed out" #t)
        (engine-kill eng)
        (reset-place-list)))))
