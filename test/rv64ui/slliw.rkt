#lang rosette
(require "../riscv/test-macros.rkt")
(provide run-riscv-slliw-tests)
(define (run-riscv-slliw-tests)
; See LICENSE for license details.

;*****************************************************************************
; 'slliw.S
;-----------------------------------------------------------------------------
;
; Test 'slliw instruction.
;

;include "riscv_test.h"
;include "test_macros.h"




  ;-------------------------------------------------------------
  ; Arithmetic tests
  ;-------------------------------------------------------------

  (TEST_IMM_OP 2  'slliw #x0000000000000001 #x0000000000000001 0  );
  (TEST_IMM_OP 3  'slliw #x0000000000000002 #x0000000000000001 1  );
  (TEST_IMM_OP 4  'slliw #x0000000000000080 #x0000000000000001 7  );
  (TEST_IMM_OP 5  'slliw #x0000000000004000 #x0000000000000001 14 );
  (TEST_IMM_OP 6  'slliw #xffffffff80000000 #x0000000000000001 31 );

  (TEST_IMM_OP 7  'slliw #xffffffffffffffff #xffffffffffffffff 0  );
  (TEST_IMM_OP 8  'slliw #xfffffffffffffffe #xffffffffffffffff 1  );
  (TEST_IMM_OP 9  'slliw #xffffffffffffff80 #xffffffffffffffff 7  );
  (TEST_IMM_OP 10 'slliw #xffffffffffffc000 #xffffffffffffffff 14 );
  (TEST_IMM_OP 11 'slliw #xffffffff80000000 #xffffffffffffffff 31 );

  (TEST_IMM_OP 12 'slliw #x0000000021212121 #x0000000021212121 0  );
  (TEST_IMM_OP 13 'slliw #x0000000042424242 #x0000000021212121 1  );
  (TEST_IMM_OP 14 'slliw #xffffffff90909080 #x0000000021212121 7  );
  (TEST_IMM_OP 15 'slliw #x0000000048484000 #x0000000021212121 14 );
  (TEST_IMM_OP 16 'slliw #xffffffff80000000 #x0000000021212121 31 );

  ; Verify that shifts ignore top 32 (using true 64-bit values)

  (TEST_IMM_OP 44 'slliw #x0000000012345678 #xffffffff12345678 0 );
  (TEST_IMM_OP 45 'slliw #x0000000023456780 #xffffffff12345678 4 );
  (TEST_IMM_OP 46 'slliw #xffffffff92345678 #x0000000092345678 0 );
  (TEST_IMM_OP 47 'slliw #xffffffff93456780 #x0000000099345678 4 );

  ;-------------------------------------------------------------
  ; Source/Destination tests
  ;-------------------------------------------------------------

  (TEST_IMM_SRC1_EQ_DEST 17 'slliw #x00000080 #x00000001 7 );

  ;-------------------------------------------------------------
  ; Bypassing tests
  ;-------------------------------------------------------------

  (TEST_IMM_DEST_BYPASS 18 0 'slliw #x0000000000000080 #x0000000000000001 7  );
  (TEST_IMM_DEST_BYPASS 19 1 'slliw #x0000000000004000 #x0000000000000001 14 );
  (TEST_IMM_DEST_BYPASS 20 2 'slliw #xffffffff80000000 #x0000000000000001 31 );

  (TEST_IMM_SRC1_BYPASS 21 0 'slliw #x0000000000000080 #x0000000000000001 7  );
  (TEST_IMM_SRC1_BYPASS 22 1 'slliw #x0000000000004000 #x0000000000000001 14 );
  (TEST_IMM_SRC1_BYPASS 23 2 'slliw #xffffffff80000000 #x0000000000000001 31 );

  (TEST_IMM_ZEROSRC1 24 'slliw 0 31 );
  (TEST_IMM_ZERODEST 25 'slliw 31 28 );

  



  


  



)