#lang rosette

(require racket/serialize)

(provide d+f->file
         read-from-dir
         write-to-dir
         stp-file->dir
         cch-file-exists)

(define (make-dir dir)
  (when (not (directory-exists? dir))
    ; TODO handle error here
    (make-directory dir)))

(define (d+f->file dirname filename input?)
  (if (not (and dirname filename))
    #f
    (let* ([dirpath (string->path dirname)]
           [filepath
             (build-path dirpath (string->path filename))])
      (if input?
        (if (file-exists? filepath)
          (open-input-file filepath)
          #f)
        (begin
          (make-dir dirname)
          (open-output-file filepath #:exists 'replace))))))
 
(define (read-from-dir dirname filename)
  (let ([file (d+f->file dirname filename #t)])
    (cond
      [file
        (define res (deserialize (read file)))
        (close-input-port file)
        res]
      [else #f])))

(define (write-to-dir dirname filename value)
  (let ([file (d+f->file dirname filename #f)])
    (when file
      (write (serialize value) file)
      (close-output-port file))
    value))

(define (stp-file->dir
          stp-file [make-dir? #t]
          #:cache-dir [cache-dir "cchh"])
  (let ([dir (~a (first (string-split stp-file "/")) "/" cache-dir)])
    (when make-dir? (make-dir dir))
    dir))

(define (cch-file-exists
          stp-file instr-name len
          #:cache-dir [cache-dir "cchh"])
  (file-exists? (~a (stp-file->dir
                      stp-file #f
                      #:cache-dir cache-dir)
                    "/" instr-name
                    "/" len ".cch")))
