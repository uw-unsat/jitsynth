#lang rosette

(require "utils.rkt")

(define lines (file->lines "linux/test/rawtests.c"))
(define out (open-output-file "linux/test/bpf-tests.rkt" #:exists 'replace))

(displayln "#lang rosette" out)
(displayln (~a "(require \"utils.rkt\" "
               "\"../../common/data-utils.rkt\" "
               "\"../../bpf/constants.rkt\")\n")
           out)
(displayln "(provide linux-bpf-test-cases)" out)
(displayln "(define linux-bpf-test-cases (list" out)

; NOTE: Ignoring Endian tests

(define (clean str)
  (string-trim
    (string-replace
      str
      "0x" "#x")
    #px"(\\s|\\{|\\}|\\(|\\)|L|U|(0,)|,|(/\\*.*\\*/))+"))

(define (pl->rl program-line)
  (let ([line (string-trim program-line)])
    (~a
      "("
      (clean
        (string-replace
          (string-replace
            line
            "," "")
          "(" " "))
      ")")))

(define program-lines null)
(define reading-program? #f)
(for ([line lines])
  (cond
    [(string-contains? line ".u.insns_int = {")
     (set! reading-program? #t)]
    [reading-program?
     (if (string-contains? line "}")
       (set! reading-program? #f)
       (set! program-lines (append program-lines (list line))))]
    [(and (string-contains? line "{ { 0,")
          (not (null? program-lines)))
     (displayln "(linux-test-case" out)
     (displayln "(flatten-list" out)
     (for ([pl program-lines])
       (displayln (pl->rl pl) out))
     (displayln ")" out)
     (displayln (~a "(hash 0 " (clean line) ")") out)
     (displayln ")" out)
     (set! program-lines null)]))

(displayln "))" out)

(close-output-port out)
