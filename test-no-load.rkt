#lang rosette

(require "synthesis/compiler-query.rkt"
         "genc/genc.rkt"
         "common/options.rkt")
(require racket/system)

(define args (current-command-line-arguments))
(define dir (~a (vector-ref args 0)))
(define stp-file
  (~a dir "/"
      (cond
        [(>= (vector-length args) 2)
         (~a (vector-ref args 1) ".rkt")]
        [else "stp.rkt"])))

(synth-compiler-parallel-with-timeout
  stp-file
  (list #f #t #t #t)
  #:cache-dir "load-cchh-off")
(create-cfile
  stp-file
  #:cache-dir "load-cchh-off")
