#lang rosette

; NOTE: Right now, I'm just outputting strings
; Do this less stupidly at some point

(require "../ams/machine.rkt"
         "../common/case-hex.rkt"
         "../common/file-utils.rkt"
         "../common/data-utils.rkt")
(require pprint
         racket/serialize)
(require (only-in rosette/base/core/polymorphic ite))
(require (only-in "../common/debug.rkt" [debugln common:debugln]))

(provide create-cfile
         tgt-prog->cfunc
         (struct-out cfunc))

(define DEBUG #t)

(define (debugln x)
  (if DEBUG (common:debugln x) void))

(define helper-impls null)
(define (add-impl impl)
  (set! helper-impls (cons impl helper-impls)))

(define all-cfuncs null)
(define (add-cfunc name)
  (set! all-cfuncs (cons name all-cfuncs)))

; TODO seems to be doing something funny to #xff0
(define (bv->text bv)
  (text (format "0x~x" (bitvector->natural bv))))

; TODO should I have some equality nonsense?
(serializable-struct cfunc (name params) #:transparent)

(define (read-cfunc dir filename)
  (read-from-dir dir filename))

(define (write-cfunc dir filename cfunc)
  (write-to-dir dir filename cfunc))

; TODO is this needed?
(define (name->text name)
  (text (~a name)))

; TODO do some magic to auto indent
(define (cfunc->text my-cfunc)
  (let* ([params (cfunc-params my-cfunc)]
         ; NOTE: I just have this in here b/c I messed up
         ;       before and cached results are screwey
         [params (if (list? params) params (list params))]
         [name (cfunc-name my-cfunc)]
         [params-docs
           (if (null? params) (text "")
             (h-append
               (cparam->text (first params))
               (h-concat 
                 ; TODO use apply-infix here
                 (for/list ([p (rest params)])
                   (h-append comma space (cparam->text p))))))])
    (when (not (has-func? name))
      (add-user-defined-func name))
    (h-append (name->text name)
              lparen
              params-docs
              rparen)))

(define (cparam->text param)
  (if (cfunc? param) (cfunc->text param)
    (if (bv? param) (bv->text param)
      (text (~a param)))))

(define input-src-instr 'si)

(define cadd-impl
  (v-append
    ; TODO extract 0-bl
    (nest 4 (v-append (text "u64 cadd(u64 a, u64 b, u64 bl) {")
                      (text "return cextract(bl - 1, 0, a + b);")))
    (text "}")))
(add-cfunc 'cadd)
(add-impl cadd-impl)

(define (cadd a b bl)
  (cfunc 'cadd (list a b bl)))

(define cmul-impl
  (v-append
    (nest 4 (v-append (text "u64 cmul(u64 a, u64 b, u64 bl) {")
                      (text "return cextract(bl - 1, 0, a * b);")))
    (text "}")))
(add-cfunc 'cmul)
(add-impl cmul-impl)

(define (cmul a b bl)
  (cfunc 'cmul (list a b bl)))

(define clshr-impl
  (v-append
    (nest 4 (v-append (text "u64 clshr(u64 a, u64 b, u64 bl) {")
                      (text "return cextract(bl - 1, 0, a >> b);")))
    (text "}")))
(add-cfunc 'clshr)
(add-impl clshr-impl)

(define (clshr a b bl)
  (cfunc 'clshr (list a b bl)))

(define cashr-impl
  (v-append
    (nest 4 (v-append (text "u64 cashr(u64 a, u64 b, u64 bl) {")
                      (text "return cextract(bl - 1, 0, ((s64) a) >> b);")))
    (text "}")))
(add-cfunc 'cashr)
(add-impl cashr-impl)

(define (cashr a b bl)
  (cfunc 'cashr (list a b bl)))

(define cshl-impl
  (v-append
    (nest 4 (v-append (text "u64 cshl(u64 a, u64 b, u64 bl) {")
                      (text "return cextract(bl - 1, 0, a << b);")))
    (text "}")))
(add-cfunc 'cshl)
(add-impl cshl-impl)

(define (cshl a b bl)
  (cfunc 'cshl (list a b bl)))

(define cxor-impl
  (v-append
    (nest 4 (v-append (text "u64 cxor(u64 a, u64 b, u64 bl) {")
                      (text "return cextract(bl - 1, 0, a ^ b);")))
    (text "}")))
(add-cfunc 'cxor)
(add-impl cxor-impl)

(define (cxor a b bl)
  (cfunc 'cxor (list a b bl)))

(define cor-impl
  (v-append
    (nest 4 (v-append (text "u64 cor(u64 a, u64 b, u64 bl) {")
                      (text "return cextract(bl - 1, 0, a | b);")))
    (text "}")))
(add-cfunc 'cor)
(add-impl cor-impl)

(define (cor a b bl)
  (cfunc 'cor (list a b bl)))

(define cand-impl
  (v-append
    (nest 4 (v-append (text "u64 cand(u64 a, u64 b, u64 bl) {")
                      (text "return cextract(bl - 1, 0, a & b);")))
    (text "}")))
(add-cfunc 'cand)
(add-impl cand-impl)

(define (cand a b bl)
  (cfunc 'cand (list a b bl)))

(define cneg-impl
  (v-append
    (nest 4 (v-append (text "u64 cneg(u64 a, u64 bl) {")
                      (text "return cextract(bl - 1, 0, -a);")))
    (text "}")))
(add-cfunc 'cneg)
(add-impl cneg-impl)

(define (cneg a bl)
  (cfunc 'cneg (list a bl)))

(define ceq-impl
  (v-append
    (nest 4 (v-append (text "bool ceq(u64 a, u64 b) {")
                      (text "return a == b;")))
    (text "}")))
(add-cfunc 'ceq)
(add-impl ceq-impl)

(define (ceq a b)
  (cfunc 'ceq (list a b)))

(define cite-impl
  (v-append
    (nest 4 (v-append (text "u64 cite(bool cond, u64 a, u64 b) {")
                      (text "return cond ? a : b;")))
    (text "}")))
(add-cfunc 'cite)
(add-impl cite-impl)

(define (cite ite-cond a b)
  (cfunc 'cite (list ite-cond a b)))

(define cconcat-impl
  (v-append
    (nest 4 (v-append (text "u64 cconcat(u64 a, u64 b, u64 abl, u64 bbl) {")
                      (text "return cextract(abl + bbl - 1, 0, a << bbl + cextract(bbl - 1, 0, b));")))
    (text "}")))
(add-cfunc 'cconcat)
(add-impl cconcat-impl)

(define (cconcat a b bl)
  (cfunc 'cconcat (list a b bl)))

(define csign-extend-impl
  (v-append
    (nest 4 (v-append (text "u64 csign_extend(u64 b, u64 l, u64 bl) {")
                      (text "return cextract(l - 1, 0, b & ((b & (1 << (bl - 1))) ? (-1 << bl) : 0));")))
    (text "}")))
(add-cfunc 'csign_extend)
(add-impl csign-extend-impl)

(define (csign-extend b l bl)
  (cfunc 'csign_extend (list b l bl)))

(define czero-extend-impl
  (v-append
    (nest 4 (v-append (text "u64 czero_extend(u64 b, u64 l, u64 bl) {")
                      (text "return cextract(l - 1, 0, b);")))
    (text "}")))
(add-cfunc 'czero_extend)
(add-impl czero-extend-impl)

(define (czero-extend b l bl)
  (cfunc 'czero_extend (list b l bl)))

; TODO is this correct?
(define cextract-impl
  (v-append
    ; TODO should I remove bl from this one?
    (nest 4 (v-append (text "u64 cextract(u64 e, u64 s, u64 b) {")
                      (text "return (b << (63 - e)) >> (63 - e + s);")))
    (text "}")))
(add-cfunc 'cextract)
(add-impl cextract-impl)

(define (cextract e s b)
  (cfunc 'cextract (list e s b)))

#|
(define tgt-insn-impl
  (v-append
    (text "// TODO Implement this function in aux.h")
    (text "// struct tgt_insn make_tgt_insn(...) {...}")))
(add-impl tgt-insn-impl)|#

(define tgt_insn
  (lambda args
    (cfunc 'make_tgt_insn args)))

(define (add-user-defined-func name)
  (define impl
    (v-append
      (text "// TODO Implement this function in aux.h")
      (text (~a "// u64 " name "(struct src_instr *si) {...}"))))
  (add-cfunc name)
  (add-impl impl))

(add-user-defined-func 'src_insn_op)

; TODO include the (mutable) target instruction here too
(define (emit x)
  (cfunc 'emit (list x)))

(define (has-func? name)
  (member name all-cfuncs))

(define (src-sym->cparam-helper sym si parameters)
  (define param (first parameters))
  (define candidate ((parameter-get param) si))
  (debugln (~a "Candidate: " candidate))
  (if (unsat? (verify (assert (bveq candidate sym))))
    (let ([fname (string->symbol (~a "src_insn_" (parameter-name param)))])
      (cfunc fname (list input-src-instr)))
    (cond
      [(empty? (rest parameters))
       (debugln (~a "Not a sym value: " sym "\n  for instruction " si))
       #f]
      [else (src-sym->cparam-helper sym si (rest parameters))])))

(define (src-sym->cparam sym si mch-src-instr)
  (debugln (~a "Src symbolic value: " sym))
  (debugln (~a "Src instr: " si))
  (src-sym->cparam-helper sym si (instruction-params mch-src-instr)))

; TODO NEXT figure out how to get mch-src-instr here
(define (value->cparam value si mch-src-instr)
  (debugln value)
  (match* (value)
    ; TODO do I need an LL somewhere?
    ;      can I just do bitvector->natural instead?
    ;      and then put an "LL" in front of it when converting to text or smth
    [((bv x _)) (cextract (sub1 (bv-size value)) 0 x)]
    [((expression (== bvadd) a b)) (cadd (value->cparam a si mch-src-instr) (value->cparam b si mch-src-instr) (bv-size a))]
    [((expression (== bvmul) a b)) (cmul (value->cparam a si mch-src-instr) (value->cparam b si mch-src-instr) (bv-size a))]
    [((expression (== bvshl) a b)) (cshl (value->cparam a si mch-src-instr) (value->cparam b si mch-src-instr) (bv-size a))]
    [((expression (== bvshl) a b)) (cshl (value->cparam a si mch-src-instr) (value->cparam b si mch-src-instr) (bv-size a))]
    [((expression (== bvashr) a b)) (cashr (value->cparam a si mch-src-instr) (value->cparam b si mch-src-instr) (bv-size a))]
    [((expression (== bvlshr) a b)) (clshr (value->cparam a si mch-src-instr) (value->cparam b si mch-src-instr) (bv-size a))]
    [((expression (== bvor) a b)) (cor (value->cparam a si mch-src-instr) (value->cparam b si mch-src-instr) (bv-size a))]
    [((expression (== bvxor) a b)) (cxor (value->cparam a si mch-src-instr) (value->cparam b si mch-src-instr) (bv-size a))]
    [((expression (== bvand) a b)) (cand (value->cparam a si mch-src-instr) (value->cparam b si mch-src-instr) (bv-size a))]
    [((expression (== bvneg) a)) (cneg (value->cparam a si mch-src-instr) (bv-size a))]
    [((expression (== concat) a b)) (cconcat (value->cparam a si mch-src-instr) (value->cparam b si mch-src-instr) (bv-size b))]
    [((expression (== extract) s e v)) (cextract s e (value->cparam v si mch-src-instr))]
    [((expression (== zero-extend) a (bitvector b)))
     (czero-extend (value->cparam a si mch-src-instr) b (bv-size a))]
    [((expression (== sign-extend) a (bitvector b)))
     (csign-extend (value->cparam a si mch-src-instr) b (bv-size a))]
    ; TODO other bvops?
    ; bveq
    [((expression (== bveq) a b))
      (ceq (value->cparam a si mch-src-instr)
           (value->cparam b si mch-src-instr))]
    ; ite
    [((expression (== ite) a b c))
     (define src-sym (src-sym->cparam value si mch-src-instr))
     (if src-sym
       src-sym
       (cite (value->cparam a si mch-src-instr)
             (value->cparam b si mch-src-instr)
             (value->cparam c si mch-src-instr)))]
    ; TODO should error when this returns #f or smth(?)
    [(_)
     (define src-sym (src-sym->cparam value si mch-src-instr))
     (when (not src-sym)
       (displayln (~a "Failed on sym value: " src-sym "\n  for instruction " si)))
     src-sym]))

(define (tgt-instr->cfunc si ti mch-src-instr tgt-instr-getters)
  (let ([val-func (lambda (f) (value->cparam (f ti) si mch-src-instr))])
    (apply tgt_insn (map val-func tgt-instr-getters))))

; More precisely, this is a list of cfuncs
(define (tgt-prog->cfunc si tgt-prog mch-src-instr target-machine)
  (map (lambda (ti)
         (let ([tgt-instr-getters (instr-getters target-machine ti)])
           (tgt-instr->cfunc si ti mch-src-instr tgt-instr-getters)))
       tgt-prog))

(define (op->text op)
  (text
    (~a
      (cond
        [(bv? op) (bitvector->natural op)]
        [(symbol? op) op]
        [else (error "op->text: op should be symbol or bitvector")]))))

(define (tgt-prog->text src-instr cfunc-tgt-prog mch-src-instr source)
  (let ([body
          (v-concat
            (for/list ([cfunc-tgt-instr cfunc-tgt-prog])
              (h-append (cparam->text (emit cfunc-tgt-instr))
                        (text ";"))))])
    (v-append 
      (text (~a "// Instruction " (instruction-name mch-src-instr)))
      (nest 4 (v-append
                (h-append (text "case") space
                          ; TODO assume that this is an ams/machine function
                          (op->text (mch-instr->distinct-op source mch-src-instr))
                          colon)
                body
                (text "break;"))))))

(define si-type "src_insn")

; TODO also include the target program as pointer in params
(define emit-insn-header
  (nest 4 (v-append (text (~a "int emit_insn(" si-type " *si) {"))
                    ; TODO make user provide "op" function
                    (text "switch (src_insn_op(si)) {"))))

(define emit-insn-tail
  (v-append (indent 4 (text "}"))
            (text "}")))

(define (helper-impl-text)
  (v-concat (apply-infix line helper-impls)))

(define guard+comments
  (v-append (text "#pragma once")
            (text (~a "/* JIT compiler"))
            (text " */")))

; TODO fill this out
(define includes 
  (v-append
    (text "#include <stdio.h>")
    (text "#include <string.h>")
    (text "#include <stdbool.h>")
    (text "#include <stdlib.h>")
    (text "#include <stdarg.h>")
    (text "#include <stdint.h>")
    (text "#include <stddef.h>")
    (text "#include \"aux.h\"")
    (text "typedef unsigned long long u64;")
    (text "typedef long long s64;")))

(define struct-defs
  (v-append
    (text "// TODO Define this struct in aux.h")
    (text "// struct src_insn {...};")
    (text "")
    (text "// TODO (Optionally) Define this struct in aux.h")
    (text "// struct tgt_insn {...};")))

(define cfile-header
  (thunk
    (v-append
      guard+comments
      empty
      includes
      empty
      struct-defs
      empty
      (helper-impl-text)
      empty
      emit-insn-header
      empty)))

(define cfile-tail
  emit-insn-tail)

; NOTE: Assuming the compiler has already been synthesized
(define (create-cfile stp-file #:cache-dir [cache-dir "cchh"])
  (let ([proglen (read-from-dir (stp-file->dir stp-file #f #:cache-dir cache-dir) "proglen.cch")])
    (cond
      [proglen
        (define output-file (~a (stp-file->dir stp-file #f #:cache-dir cache-dir) "/compiler.h"))
        (define stp ((dynamic-require stp-file 'make-stp)))
        (define source (src/tgt-source stp))
        (define target (src/tgt-target stp))
        (define source-instrs (machine-instrs source))
        (define bodies
          (map
            (lambda (src-instr)
              (compute-body stp-file stp src-instr #:cache-dir cache-dir))
            source-instrs))
        (define output-port (start-cfile output-file))
        (for ([body bodies])
          (update-cfile body output-port))
        (end-cfile output-port)]
      [else
        (displayln "C file not made because synthesis failed")])))

(define (start-cfile output-file)
  (let ([port (open-output-file output-file #:exists 'replace)])
    (pretty-print (cfile-header) port)
    (close-output-port port)
    (open-output-file output-file #:exists 'append)))

(define (compute-body stp-file src/tgt-pair src-instr #:cache-dir cache-dir)
  (let* ([source (src/tgt-source src/tgt-pair)]
         [target (src/tgt-target src/tgt-pair)]
         [canon-src-instr (make-canon-instr source src-instr)]
         [proglen (read-from-dir (stp-file->dir stp-file #f #:cache-dir cache-dir) "proglen.cch")]
         [dir (~a (stp-file->dir stp-file #f #:cache-dir cache-dir) "/" (instruction-name src-instr))]
         [tgt-prog (read-from-dir dir (~a proglen ".cch"))]
         [body (tgt-prog->text canon-src-instr tgt-prog src-instr source)])
    body))

(define (update-cfile body output-port)
  (pretty-print
    (h-append (indent 8 body) line)
    output-port)
  (flush-output output-port))

(define (end-cfile output-port)
  (pretty-print cfile-tail output-port)
  (close-output-port output-port))
