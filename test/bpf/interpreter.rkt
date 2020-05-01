#lang rosette

(require "../../bpf/lang/parser.rkt"
         "../../bpf/lang/op.rkt"
         "../../bpf/lang/transformer.rkt"
         "../../bpf/interpreter.rkt"
         "../../common/data-structures.rkt"
         "../../common/instructions.rkt"
         "../../common/symbolic-ops.rkt")

(require rackunit)

(require (only-in "../../common/debug.rkt" [debugln common:debugln]))

(define DEBUG #f)

(define (debugln x)
  (if DEBUG (common:debugln x) x))

(provide bpf-interpreter-tests)

(struct test (asm raw c mem error result) #:transparent)

(define (update-map my-map key val)
  (if (not (hash-ref my-map key))
    (hash-set my-map key (list val))
    (hash-set my-map key (cons val (hash-ref my-map key)))))

(define (read-asm rev-lines)
  (if (not rev-lines) #f
      (let ([lines (reverse rev-lines)])
        (map (lambda (x) (list-ref x 1))
             (filter (lambda (x) (not (void? x)))
                     (map string->datum lines))))))

(define (read-raw rev-lines)
  (if (not rev-lines) #f
      (let ([lines (reverse rev-lines)])
        (map (lambda (str) (string->bv (substring str 2) 16 64)) lines))))

(define (read-c rev-lines)
  (if (not rev-lines) #f
      (let ([lines (reverse rev-lines)])
        lines)))

(define (read-mem rev-lines)
  (if (not rev-lines) #f
      (let* ([lines (reverse rev-lines)]
             [textstream (apply append (map string-split lines))])
        (make-memory-from-list (map (lambda (s) (string->bv s 16 8)) textstream)))))

(define (read-error lines)
  (if (not lines) #f
      (if (> (length lines) 1) (error "Too many lines in result.")
          (let ([line (first lines)])
            line))))

(define (read-result lines)
  (if (not lines) #f
      (if (> (length lines) 1) (error "Too many lines in result.")
          (let ([line (first lines)])
            (zero-extend (string->bv (substring line 2) 16 (* (- (string-length line) 2) 4)) (bitvector 64))))))

(define (make-test file)
  (define in (open-input-file file))

  (define input-cases (list "asm" "raw" "c" "mem" "error" "result"))

  (define reading #f)
  (define test-case-map (hash 'asm #f 'raw #f 'c #f 'mem #f 'error #f 'result #f))
  
  (for ([line (sequence-map string-trim (in-lines in))])
    (if (member line (map (lambda (s) (string-append "-- " s)) input-cases))
        (set! reading (substring line 3))
        (if (not reading) (error "Invalid test case input")
            (set! test-case-map (update-map test-case-map (string->symbol reading) line)))))

  (test (read-asm (hash-ref test-case-map 'asm))
        (read-raw (hash-ref test-case-map 'raw))
        (read-c (hash-ref test-case-map 'c))
        (read-mem (hash-ref test-case-map 'mem))
        (read-error (hash-ref test-case-map 'error))
        (read-result (hash-ref test-case-map 'result))))

; NOTE: RISC-V tests commented out for now since not all instructions are implemented
;       Detailed (but less readable) output also left in comments
(define (run-test file)
  (let* ([test-case (make-test file)]
         [code-lines (test-asm test-case)]
         [code/raw (if (not code-lines) #f
                       ; transform the unquoted lines, (list-ref x 1) unquotes
                       (transform-raw code-lines))]
         [bpf-instrs (if (not code/raw) #f
                       (map raw->bpf-instr code/raw))]
         [memory (if (test-mem test-case)
                   (test-mem test-case)
                   (make-zerod-memory 128))]
         [bpf-interp-result (if (and code/raw (test-result test-case))
                              (get-return-value
                                (interpret-with-state
                                  bpf-instrs
                                  (make-zerod-registers 11)
                                  memory
                                  (bv 0 64)
                                  #:allow-bele? #t))
                                #f)])
    ; TODO
    (debugln file)
    ; Check for the correct result
    (if (and bpf-interp-result (test-result test-case))
      (check-equal? bpf-interp-result (test-result test-case))
      (void))))

; Run test suite with BPF interpreter
(define test-folder (string->path "test/ubpf/"))
(define-test-suite bpf-interpreter-tests
  (unset-sym)
  (for ([p (filter
             (lambda (p) (equal? #".data" (path-get-extension p)))
             (directory-list test-folder))])
    (run-test (path->string (build-path test-folder p)))))
