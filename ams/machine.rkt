#lang rosette

(require "../common/data-utils.rkt"
         "../common/data-structures.rkt")
(require (only-in "../common/debug.rkt" [debugln common:debugln]))
(require rosette/lib/angelic)

(provide iws-from-am instr-getters
         get-instr-bitlen get-instr-as-bv
         make-symbolic-instr make-canon-instr
         get-numregs get-regbits get-membits
         get-read-set get-write-set
         get-param-write-set get-instrs-for-rs+ws
         get-instrs-compl get-instrs-for-ws get-instrs-for-strict-ws
         get-instrs-for-rs+ws-write-strict get-instrs-for-rs+ws-strict
         get-source get-target get-scratch-regs state-equal?
         state-regs-equal? state-mem-equal? state-pc-equal?
         get-iwss has-instr? get-instr t2s-state
         get-parameter get-parameter-value
         get-source-param-bitlen get-regs-bitlen
         get-equality-indicies make-nop-state
         apply-state-asmts apply-state-invars
         is-reg? is-value? get-param-bitlen
         make-state state-rvals state-memory
         mch-instr->distinct-op
         (struct-out state) (struct-out machine)
         (struct-out instruction) (struct-out parameter)
         (struct-out src/tgt))

(define DEBUG #f)

(define (debugln x)
  (if DEBUG (common:debugln x) void))

(struct state (regs mem pc return?) #:transparent)

(define (make-state regs mem pc [return? #f])
  (state regs mem pc return?))

(define (state-rvals st)
  (state-regs st))

(define (state-memory st)
  (state-mem st))

(struct machine
  (default-state        ; a general abstract state. Any concrete state should occupy this state
   state-asmt-func      ; function of machine state that generates assumptions about the state
   state-invar-func     ; function of machine state that generates invariant assertions
   interpret-with-state ; method that interprets a machine program with a starting state
   instrs               ; list of machine instructions
   nopi                 ; concrete instruction that performs no operation
   instr->getters       ; map from concrete instruction to parameter getter function
   canon-instr-cache    ; cache for canon parameters (parameter values to be reused)
   reg-sym-index        ; symbolic index of a register, can be used for measuring register equality
   mem-sym-index        ; symbolic index for memory, can be used for measuring memory equality
   ws->instrs           ; function that gives the instructions that have a particular write set
   rs->instrs)          ; function that gives the instructions that have a particular read set 
  #:transparent)

; Represents a class of instructions
(struct instruction
  (name        ; symbol name for the instruction
   make        ; function that creates an instruction given the components
   params      ; list of parameters for the instruction
   read-set    ; set of state values that the instruction may read from
   write-set)  ; set of state values that the instruction may write to
  #:transparent)

(struct parameter
  (name
   type          ; value, reg, or op
   bitlen        ; number of bits that represent this arg, -1 if not a bitvector
   get           ; function that takes an instruction and parameter name,
                 ; returning the value of the named parameter
   value-func    ; a function of fresh symbolic values that represents
                 ; all possible values for the parameter
   cachable?     ; able to cache the value of these parameters?
   write-set)    ; set of state values that the parameter may affect
                 ; (this is always a subset of the parent instruction's write set)
  #:transparent)

; Describes a source & taarget language pair
(struct src/tgt
  (source        ; Source machine
   target        ; Target machine
   scratch-regs  ; Target scratch registers
   usable-regs   ; Registers that the target language is allowed to use
   s2t-reg-index ; Mapping from source to target register index
   t2s-regs      ; Register mapping from target to source
   t2s-mem       ; Memory mapping from target to source
   t2s-pc        ; PC mapping from target to source
   pc-fact       ; Helper info, Z+ factor to multiply source PC by to get target PC
   src-param-bitlen)
  #:transparent)

(define (instr-getters am conc-instr)
  ((machine-instr->getters am) conc-instr))

; Possible options for type are 'regs and 'imm
(define (get-reg-sym am)
  (let ([regbits (get-regbits am)]
        [numregs (get-numregs am)])
    (if (equal? (expt 2 (get-regbits am)) (get-numregs am))
      (symbv (get-regbits am))
      (apply choose* (build-list numregs (bvl regbits))))))

(define (get-numregs am)
  (bitvector->natural (numregs (state-regs (machine-default-state am)))))

(define (get-regbits am)
  (exact-ceiling (log (get-numregs am) 2)))

(define (get-regs-bitlen am)
  (regs-bitlen (state-regs (machine-default-state am))))

(define (get-mem-size am)
  (bitvector->natural (mem-size (state-mem (machine-default-state am)))))

(define (get-membits am)
  (exact-ceiling (log (get-mem-size am) 2)))

(define (get-pcbits machine) 64)

; NOTE: These equality functions describe equality between 
;       regs/mem/pc of the same state
(define (state-regs-equal? am r1 r2)
  (let ([index (machine-reg-sym-index am)])
    (debugln (~a "Num regs: " (numregs r1)))
    (implies
      (bvult (zero-extend index (bitvector 8)) (numregs r1))
      (bveq (read-register r1 index)
            (read-register r2 index)))))

(define (state-mem-equal? am m1 m2)
  (let ([index (machine-mem-sym-index am)])
    (implies
      (bvult (zero-extend index (bitvector 64)) (mem-size m1))
      (bveq (read-memory m1 index (bv 1 8))
            (read-memory m2 index (bv 1 8))))))

(define (state-pc-equal? pc1 pc2)
  (bveq pc1 pc2))

(define (get-equality-indicies am)
  (list (machine-reg-sym-index am) (machine-mem-sym-index am)))

(define (state-equal? am s1 s2)
  (and
    (state-regs-equal?
      am
      (state-regs s1)
      (state-regs s2))
    (state-mem-equal?
      am
      (state-mem s1)
      (state-mem s2))
    (state-pc-equal?
      (state-pc s1)
      (state-pc s2))
    (equal?
      (state-return? s1)
      (state-return? s2))))

(define (make-symbolic-instr instr)
  (apply
    (instruction-make instr)
    (for/list ([param (instruction-params instr)])
      ((parameter-value-func param)))))

(define (make-canon-instr am instr)
  (let ([cache (machine-canon-instr-cache am)])
    (apply
      (instruction-make instr)
      (for/list ([param (instruction-params instr)])
        (let ([value ((parameter-value-func param))])
          (if (and (parameter-cachable? param)
                   (not (empty? (symbolics value))))
            (if (hash-has-key? cache (parameter-name param))
              (hash-ref cache (parameter-name param))
              (begin
                (hash-set! cache (parameter-name param) value)
                value))
            value))))))

(define (get-param-write-set param)
  (parameter-write-set param))

(define (get-write-set instr)
  (instruction-write-set instr))

(define (get-read-set instr)
  (instruction-read-set instr))

(define (get-instrs-compl am compl-instrs)
  (remove* compl-instrs (machine-instrs am)))

; get-instrs-for-rs+ws returns all instructions who read at least the given read set
;                              and write no more than the given write set
(define (get-instrs-for-rs+ws am rs ws)
  (filter
    (lambda (instr)
      (let ([instr-rs (instruction-read-set instr)]
            [instr-ws (instruction-write-set instr)])
        (and (sublist? rs instr-rs)
             (sublist? instr-ws ws))))
    (machine-instrs am)))

; Returns all ws that write a subset of the given state space (ws)
(define (get-instrs-for-ws am ws)
  (filter
    (lambda (instr)
      (let ([instr-ws (instruction-write-set instr)])
        (sublist? instr-ws (cons 'regs ws))))
    (machine-instrs am)))

; Returns all ws that write a superset of the given state space (ws)
(define (get-instrs-for-strict-ws am ws)
  (filter
    (lambda (instr)
      (let ([instr-ws (instruction-write-set instr)])
        (sublist? ws instr-ws)))
    (machine-instrs am)))

(define (get-instrs-for-rs+ws-write-strict am rs ws)
  (filter
    (lambda (instr)
      (let ([instr-rs (instruction-read-set instr)]
            [instr-ws (instruction-write-set instr)])
        (and (sublist? rs instr-rs)
             (set-equal? instr-ws ws))))
    (machine-instrs am)))

; get-instrs-for-rs+ws-strict returns all instructions who read exactly the given read set
;                                     and write exactly the given write set
(define (get-instrs-for-rs+ws-strict am rs ws)
  (filter
    (lambda (instr)
      (let ([instr-rs (instruction-read-set instr)]
            [instr-ws (instruction-write-set instr)])
        (and (set-equal? rs instr-rs)
             (set-equal? ws instr-ws))))
    (machine-instrs am)))

(define (iws-from-am am instr st [max-depth (bv -1 64)])
  ((machine-interpret-with-state am) instr state max-depth))

(define (get-source stpair)
  (src/tgt-source stpair))

(define (get-target stpair)
  (src/tgt-target stpair))

(define (get-scratch-regs stpair)
  (src/tgt-scratch-regs stpair))

(define (get-source-param-bitlen stpair)
  (src/tgt-src-param-bitlen stpair))

; iwss = "interpret with struct state", as opposed to components written out
(define (get-iwss ams)
  (lambda (instrs st max-depth)
    ((machine-interpret-with-state ams)
     instrs
     (state-regs st)
     (state-mem st)
     (state-pc st)
     max-depth)))

(define (make-nop-state am proglen)
  ((get-iwss am)
   (make-list proglen (machine-nopi am))
   (machine-default-state am)
   (bv64 proglen)))

(define (has-instr? ams name)
  (let ([instr-list
          (filter
            (lambda (i)
              (equal? name (instruction-name i)))
            (machine-instrs ams))])
    (not (equal? 0 (length instr-list)))))

(define (get-instr ams name)
  (let ([instr-list
          (filter
            (lambda (i)
              (equal? name (instruction-name i)))
            (machine-instrs ams))])
    (if (equal? 0 (length instr-list))
      (error (~a "No instructions of name " name))
      (if (> (length instr-list) 1)
        (error (~a "Many instructions of name " name))
        (first instr-list)))))

(define (get-parameter instr name)
  (let ([parameter-list
          (filter
            (lambda (p)
              (equal? name (parameter-name p)))
            (instruction-params instr))])
    (if (equal? 0 (length parameter-list))
      (error (~a "No parameters of name " name))
      (if (> (length parameter-list) 1)
        (error (~a "Many parameters of name " name))
        (first parameter-list)))))

(define (get-parameter-value instr name)
  (let ([param (get-parameter instr name)])
    ((parameter-value-func param))))

(define (t2s-state src/tgt-pair tgt-state)
  (state
    ((src/tgt-t2s-regs src/tgt-pair) (state-regs tgt-state))
    ((src/tgt-t2s-mem src/tgt-pair) (state-mem tgt-state))
    ((src/tgt-t2s-pc src/tgt-pair) (state-pc tgt-state))
    (state-return? tgt-state)))

(define (apply-state-invars am state)
  ((machine-state-invar-func am) state))

(define (apply-state-asmts am state)
  ((machine-state-asmt-func am) state))

(define (is-reg? param)
  (equal? 'reg (parameter-type param)))

(define (is-value? param)
  (equal? 'value (parameter-type param)))

(define (get-param-bitlen param)
  (parameter-bitlen param))

(define (get-param-bitlen-by-value param)
  (define val ((parameter-value-func param)))
  (debugln (~a "param bitlen val: " val))
  (if (bitvector? (type-of val))
    (bv-size val)
    -1))

(define (get-instr-bitlen instr)
  (foldr + 0 (filter positive? (map parameter-bitlen (instruction-params instr)))))

(define (get-instr-as-bv instr iwsable-instr)
  (let ([bvs
          (map (lambda (p) ((parameter-get p) iwsable-instr))
               (filter
                 (lambda (p) (positive? (parameter-bitlen p)))
                 (instruction-params instr)))])
    (apply concat bvs)))

(define (mch-instr->distinct-op am instr)
  (bv (index-of (machine-instrs am) instr) 64))
