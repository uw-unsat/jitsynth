#lang rosette

(provide (all-defined-out))

; dst and src are both registers (4 bit), offset is jump offset (16 bit), imm is immediate constant (32 bit)
(struct op (name dst src offset imm) #:transparent)
(struct memory (register offset) #:transparent)

(define (op->list cmd) (list (op-name cmd) (op-dst cmd) (op-src cmd) (op-offset cmd) (op-imm cmd)))
(define (list->op lst) (op (list-ref lst 0) (list-ref lst 1) (list-ref lst 2) (list-ref lst 3) (list-ref lst 4)))

(define (op->raw cmd) (concat (if (op-imm cmd) (op-imm cmd) (bv 0 32))
                              (if (op-offset cmd) (integer->bitvector (op-offset cmd) (bitvector 16)) (bv 0 16))
                              (if (op-src cmd) (integer->bitvector (op-src cmd) (bitvector 4)) (bv 0 4))
                              (if (op-dst cmd) (integer->bitvector (op-dst cmd) (bitvector 4)) (bv 0 4))
                              (integer->bitvector
                               (let ([imm? (op-imm cmd)])
                                 (case (op-name cmd)
                                   [("add") (if imm? #x07 #x0f)]
                                   [("sub") (if imm? #x17 #x1f)]
                                   [("mul") (if imm? #x27 #x2f)]
                                   [("div") (if imm? #x37 #x3f)]
                                   [("or") (if imm? #x47 #x4f)]
                                   [("and") (if imm? #x57 #x5f)]
                                   [("lsh") (if imm? #x67 #x6f)]
                                   [("rsh") (if imm? #x77 #x7f)]
                                   [("neg") #x87]
                                   [("mod") (if imm? #x97 #x9f)]
                                   [("xor") (if imm? #xa7 #xaf)]
                                   [("mov") (if imm? #xb7 #xbf)]
                                   [("arsh") (if imm? #xc7 #xcf)]
                                   
                                   [("add32") (if imm? #x04 #x0c)]
                                   [("sub32") (if imm? #x14 #x1c)]
                                   [("mul32") (if imm? #x24 #x2c)]
                                   [("div32") (if imm? #x34 #x3c)]
                                   [("or32") (if imm? #x44 #x4c)]
                                   [("and32") (if imm? #x54 #x5c)]
                                   [("lsh32") (if imm? #x64 #x6c)]
                                   [("rsh32") (if imm? #x74 #x7c)]
                                   [("neg32") #x84]
                                   [("mod32") (if imm? #x94 #x9c)]
                                   [("xor32") (if imm? #xa4 #xac)]
                                   [("mov32") (if imm? #xb4 #xbc)]
                                   [("arsh32") (if imm? #xc4 #xcc)]

                                   [("le16" "le32" "le64") #xd4] ; number is immediate value
                                   [("be16" "be32" "be64") #xdc]

                                   [("lddw") #x18]
                                   [("ldabsw") #x20]
                                   [("ldabsh") #x28]
                                   [("ldabsb") #x30]
                                   [("ldabsdw") #x38]
                                   [("ldindw") #x40]
                                   [("ldindh") #x48]
                                   [("ldindb") #x50]
                                   [("ldinddw") #x58]
                                   [("ldxw") #x61]
                                   [("ldxh") #x69]
                                   [("ldxb") #x71]
                                   [("ldxdw") #x79]
                                   [("stw") #x62]
                                   [("sth") #x6a]
                                   [("stb") #x72]
                                   [("stdw") #x7a]
                                   [("stxw") #x63]
                                   [("stxh") #x6b]
                                   [("stxb") #x73]
                                   [("stxdw") #x7b]

                                   [("ja") #x05]
                                   [("jeq") (if imm? #x15 #x1d)]
                                   [("jgt") (if imm? #x25 #x2d)]
                                   [("jge") (if imm? #x35 #x3d)]
                                   [("jlt") (if imm? #xa5 #xad)]
                                   [("jle") (if imm? #xb5 #xbd)]
                                   [("jset") (if imm? #x45 #x4d)]
                                   [("jne") (if imm? #x55 #x5d)]
                                   [("jsgt") (if imm? #x65 #x6d)]
                                   [("jsge") (if imm? #x75 #x7d)]
                                   [("jslt") (if imm? #xc5 #xcd)]
                                   [("jsle") (if imm? #xd5 #xdd)]
                                   [("call") #x85]
                                   [("exit") #x95]
                                   ))
                               (bitvector 8))))

; TODO make sure that certain values are #f instead of 0 (e.g. offset on alu64) for purposes of equality checking
(define (raw->op raw) (let* ([cls (extract 2 0 raw)]
                             [srcbit (extract 3 3 raw)]
                             [opcode (extract 7 0 raw)]
                             [dst? (not (or (bveq opcode (bv #x05 8))
                                            (bveq opcode (bv #x85 8))
                                            (bveq opcode (bv #x95 8))))]
                             [src? (or
                                    (and
                                     (or (bveq cls (bv 4 3))
                                         (bveq cls (bv 5 3))
                                         (bveq cls (bv 7 3)))
                                     (bveq srcbit (bv 1 1))
                                     (not (bveq opcode (bv #xdc 8))))
                                    (and
                                     (or (bveq cls (bv 0 3))
                                         (bveq cls (bv 1 3))
                                         (bveq cls (bv 3 3)))
                                     (not (bveq opcode (bv #x18 8)))))]
                             [offset? (and (or (bveq cls (bv 1 3))
                                               (bveq cls (bv 2 3))
                                               (bveq cls (bv 3 3))
                                               (bveq cls (bv 5 3)))
                                               (not (bveq opcode (bv #x85 8)))
                                               (not (bveq opcode (bv #x95 8))))]
                             [imm? (or
                                    (and
                                     (or (bveq cls (bv 4 3))
                                         (bveq cls (bv 5 3))
                                         (bveq cls (bv 7 3)))
                                     (bveq srcbit (bv 0 1))
                                     (not (bveq opcode (bv #x05 8)))
                                     (not (bveq opcode (bv #x95 8))))
                                    (bveq opcode (bv #xdc 8))
                                    (bveq cls (bv 0 3))
                                    (bveq cls (bv 2 3)))]
                             [imm (extract 63 32 raw)]
                             [opcode (bitvector->natural (extract 7 0 raw))])
                        (op
                         (case opcode
                           [(#x07 #x0f) "add"]
                           [(#x17 #x1f) "sub"]
                           [(#x27 #x2f) "mul"]
                           [(#x37 #x3f) "div"]
                           [(#x47 #x4f) "or"]
                           [(#x57 #x5f) "and"]
                           [(#x67 #x6f) "lsh"]
                           [(#x77 #x7f) "rsh"]
                           [(#x87) "neg"]
                           [(#x97 #x9f) "mod"]
                           [(#xa7 #xaf) "xor"]
                           [(#xb7 #xbf) "mov"]
                           [(#xc7 #xcf) "arsh"]
                                   
                           [(#x04 #x0c) "add32"]
                           [(#x14 #x1c) "sub32"]
                           [(#x24 #x2c) "mul32"]
                           [(#x34 #x3c) "div32"]
                           [(#x44 #x4c) "or32"]
                           [(#x54 #x5c) "and32"]
                           [(#x64 #x6c) "lsh32"]
                           [(#x74 #x7c) "rsh32"]
                           [(#x84) "neg32"]
                           [(#x94 #x9c) "mod32"]
                           [(#xa4 #xac) "xor32"]
                           [(#xb4 #xbc) "mov32"]
                           [(#xc4 #xcc) "arsh32"]

                           [(#xd4) (case (bitvector->natural imm)
                                     [(16) "le16"]
                                     [(32) "le32"]
                                     [(64) "le64"])]
                           [(#xdc) (case (bitvector->natural imm)
                                     [(16) "be16"]
                                     [(32) "be32"]
                                     [(64) "be64"])]

                           [(#x18) "lddw"]
                           [(#x20) "ldabsw"]
                           [(#x28) "ldabsh"]
                           [(#x30) "ldabsb"]
                           [(#x38) "ldabsdw"]
                           [(#x40) "ldindw"]
                           [(#x48) "ldindh"]
                           [(#x50) "ldindb"]
                           [(#x58) "ldinddw"]
                           [(#x61) "ldxw"]
                           [(#x69) "ldxh"]
                           [(#x71) "ldxb"]
                           [(#x79) "ldxdw"]
                           [(#x62) "stw"]
                           [(#x6a) "sth"]
                           [(#x72) "stb"]
                           [(#x7a) "stdw"]
                           [(#x63) "stxw"]
                           [(#x6b) "stxh"]
                           [(#x73) "stxb"]
                           [(#x7b) "stxdw"]

                           [(#x05) "ja"]
                           [(#x15 #x1d) "jeq"]
                           [(#x25 #x2d) "jgt"]
                           [(#x35 #x3d) "jge"]
                           [(#xa5 #xad) "jlt"]
                           [(#xb5 #xbd) "jle"]
                           [(#x45 #x4d) "jset"]
                           [(#x55 #x5d) "jne"]
                           [(#x65 #x6d) "jsgt"]
                           [(#x75 #x7d) "jsge"]
                           [(#xc5 #xcd) "jslt"]
                           [(#xd5 #xdd) "jsle"]
                           [(#x85) "call"]
                           [(#x95) "exit"]
                           )
                         (if dst? (bitvector->natural (extract 11 8 raw)) #f)
                         (if src? (bitvector->natural (extract 15 12 raw)) #f)
                         (if offset? (bitvector->integer (extract 31 16 raw)) #f)
                         (if imm? imm #f))))
