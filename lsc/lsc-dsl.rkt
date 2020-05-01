#lang rosette

(provide (all-defined-out))

; TODO enum for comparison ops
; TODO enum for actions
;
; TODO text translator/dsl rule generator

; arg: unsigned int, argument number (0 = a0)
; op: comparison operator (TODO why is this here?)
; mask: 64-bit int (NOTE: Ignoring for now)
; datum: 64-bit int (NOTE: Treating this as the constant in the op, probably not correct)
; Make tree using list structure
(struct db-api-arg (arg op mask datum valid) #:transparent)

; action: unsigned int (have map somewhere, see docs) (NOTE: Only handling ALLOW currently)
; syscall: int (TODO should make db-syscall instead?)
; strict: bool (NOTE: ignore for now)
; args: list of db-api-arg
; Make a list using actual lists
(struct db-api-rule (action syscall strict args) #:transparent)

; num: int (system call number)
; priority: int (higher is better, though not sure how to use)
; chains: tree of args (NOTE: This isn't mirrored perfectly in LSC impl)
(struct db-syscall (num priority chains) #:transparent)

; syscalls: list of db-syscall (NOTE: Ignoring for now)
; rules: list of db-api-rule
(struct db-filter (syscalls rules) #:transparent)

; NOTE: I will probably ignore these options for now
; act-default: 32-bit int, action to take if no explicit allow/deny
; act-badarch: 32-bit int, action to take if arch doesn't match
; nnp-enable: 32-bit int, related to NO_NEW_PRIVS (TODO ??)
; tsynch-enable: 32-bit int, related to SECCOMP_FILTER_FLAG_TSYNC (TODO ??)
; api-tskip: 32-bit int, allow rule with -1 syscall value
; log-enable: 32-bit int, related to SECCOMP_FILTER_FLAG_LOG (TODO ??)
(struct db-filter-attr (act-default act-badarch nnp-enable tsynch-enable api-tskip log-enable) #:transparent)

; attr: db-filter-attr
; filters: list of db-filter
(struct db-filter-col (attr filters) #:transparent)
