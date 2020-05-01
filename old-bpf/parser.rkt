#lang rosette

(require "../common/data-utils.rkt")
(require file/glob)

(define (parse-op opcode)
  (let ([op (string->symbol opcode)])
    (bv16
      ; TODO these are made-up
      (case op
        [(add) 0]
        [(sub) 1]
        [(mul) 2]
        [(div) 3]
        [(mod) 4]
        [(and) 5]
        [(or) 6]
        [(xor) 7]
        [(lsh) 8]
        [(rsh) 9]
        [(tax) 10]
        [(txa) 11]
        ; TODO implement manually?
        [(ret) 12]
        [(ld) 13]
        [(ldi) 14]
        [(ldh) 15]
        [(ldb) 16]
        [(ldx) 17]
        [(ldxi) 18]
        [(ldxb) 19]
        [(st) 20]
        [(stx) 21]
        [(jmp) 22]
        [(ja) 23]
        [(jeq) 24]
        [(jneq) 25]
        [(jne) 26]
        [(jlt) 27]
        [(jle) 28]
        [(jgt) 29]
        [(jge) 30]
        [(jset) 31]))))

; Input start and end labels as strings
(define (parse-off start end)
  (let ([sl (string->number (substring start 1))]
        [el (string->number (substring end 1))])
    ; NOTE: Subtract a 1 here to reflect that offset 1 just goes to next instruction
    (bv8 (- el sl 1))))

; Input immediate as string
(define (parse-imm imm)
  (let ([trim-imm (string-trim imm #px"(\\s|\\[|\\]|#|(0x))+")])
    (bv32
      (cond
        [(and (> (string-length imm) 2)
              (equal? "#0x" (substring imm 0 3)))
         (string->number trim-imm 16)]
        [else (string->number trim-imm)]))))

(define (parse line)
  (let* ([instr-args (string-split line #px"(\\s|,|:)+")]
         [args (lambda (i) (list-ref instr-args i))]
         [has-off? (> (length instr-args) 3)])
    (displayln instr-args)
    (concat
      (parse-imm (args 2))
      (if has-off? (parse-off (args 0) (args 3)) (bv8 0))
      (if has-off? (parse-off (args 0) (args 4)) (bv8 0))
      (parse-op (args 1)))))

(define (obpf->bvs file)
  (let* ([port (open-input-file file)]
         [lines
           (let f ()
             (let ([line (read-line port)])
               (if (eof-object? line) null (cons line (f)))))])
    (map parse lines)))

(define (print-bv my-bv port)
  (let ([str (~a my-bv)])
    (displayln (substring str 6 (- (string-length str) 4)) port)))

(define (obpf->bytestrings file)
  (let* ([bvs (obpf->bvs file)]
         [output-file (~a file "_bvs")]
         [port (open-output-file output-file #:exists 'replace)])
    (displayln (length bvs) port)
    (map (lambda (my-bv) (print-bv my-bv port)) bvs)
    (close-output-port port)))

; TODO do this for all the files next
(define dir "linux/benchmarks/jitk")
(define file-names (glob (~a dir "/*.bpf")))
(for ([fn file-names])
  (obpf->bytestrings fn)) ; "linux/benchmarks/jitk/chrome.bpf")
