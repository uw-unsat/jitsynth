#lang rosette

(define (string->datum str [quote? #t])
  (unless (equal? str "")
    (read (open-input-string
           (format (if quote? "'(~a)" "(~a)")
                   (string-replace (string-replace (string-replace
                      (string-replace (string-replace (string-replace
                         ; Remove comments
                         (string-replace str #px"#.*" "")
                         ; Make , a delimiter as well.
                         ; While this isn't exactly how BPF was originally
                         ; implemented, it's functionally equivalent
                         "," " ")
                        ; remove whitespace inside []
                        #px"\\s+(?=[^\\[\\]]*\\])" "")
                       ; replace whitespace inside () with , (for abstract computations)
                       ; NOTE: Doesn't work for nested parens
                       #px"\\s+(?=[^\\(\\)]*\\))" ",") #px"\\(," "(") #px",\\)" ")")
                    ; escape racket special characters
                    #px"(?=[#\\[\\]\\(\\)\\{\\}\"',`;\\|\\\\])" "\\"))))))

(provide string->datum)
