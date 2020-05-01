#lang rosette

(require "bpf/exec-count.rkt"
         "ams/machine.rkt"
         "common/data-structures.rkt"
         "common/data-utils.rkt"
         "common/case-hex.rkt"
         "common/instructions.rkt"
         "common/symbolic-ops.rkt")

(require (only-in "common/debug.rkt" [debugln common:debugln]))

(require file/glob)

(define DEBUG #f)

(define (debugln x)
  (if DEBUG (common:debugln x) void))

(define (parse-line line)
  (raw->bpf-instr (bv64 (string->number line 16))))

(define (file-name prefix)
  (~a "graphs/" prefix ".csv"))

(define file-prefix
  (list "l2b-fig8" "l2b-fig8" "o2b-fig8" "o2b-fig8"))

(define compilers
  (list "seccomp" "jitsynth" "linux" "jitsynth"))

(define l2b-outfile (open-output-file (file-name "l2b-fig8") #:exists 'replace))
(define o2b-outfile (open-output-file (file-name "o2b-fig8") #:exists 'replace))

(define outfiles
  (list l2b-outfile l2b-outfile o2b-outfile o2b-outfile))

(define dirs
  (list
    "l2b-bench/linux"
    "l2b-bench/jitsynth"
    "o2b-bench/linux"
    "o2b-bench/jitsynth"))

(define mem-values
  (list 0 0 #xC000003E #xC000003E))

(displayln "benchmark,compiler,instructions" l2b-outfile)
(displayln "benchmark,compiler,instructions" o2b-outfile)

(for ([prefix file-prefix]
      [compiler compilers]
      [outfile outfiles]
      [dir dirs]
      [mem-value mem-values])
  (displayln (~a "Writing to " (file-name prefix) " with a " compiler " compiler."))
  ; (displayln (~a "benchmark," prefix) outfile)
  (define glob-file-names (glob (~a dir "/*.ebpf")))
  (for ([glob-fn glob-file-names])
    (define benchmark (last (string-split (path->string glob-fn) #px"(/|(\\.ebpf))+")))
    (define fin (open-input-file glob-fn))
    (define prog
      (let f ()
        (let ([line (read-line fin)])
          (if (eof-object? line)
            null
            (cons (parse-line line) (f))))))
    (display (~a benchmark ",") outfile)
    (display (~a compiler ",") outfile)
    (interpret-with-state
      prog
      (make-zerod-registers 12)
      (write-memory
        (make-zerod-memory 128)
        (bv8 4)
        (bv8 4)
        (bv32 mem-value))
      (bv64 0)
      #:outfile outfile)))

(close-output-port l2b-outfile)
(close-output-port o2b-outfile)
