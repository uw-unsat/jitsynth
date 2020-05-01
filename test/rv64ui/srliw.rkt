#lang rosette
(require "../riscv/test-macros.rkt")
(provide run-riscv-srliw-tests)
(define (run-riscv-srliw-tests)
; See LICENSE for license details.

;*****************************************************************************
; 'srliw.S
;-----------------------------------------------------------------------------
;
; Test 'srliw instruction.
;

;include "riscv_test.h"
;include "test_macros.h"




  ;-------------------------------------------------------------
  ; Arithmetic tests
  ;-------------------------------------------------------------

  (TEST_IMM_OP 2  'srliw #xffffffff80000000 #xffffffff80000000 0  );
  (TEST_IMM_OP 3  'srliw #x0000000040000000 #xffffffff80000000 1  );
  (TEST_IMM_OP 4  'srliw #x0000000001000000 #xffffffff80000000 7  );
  (TEST_IMM_OP 5  'srliw #x0000000000020000 #xffffffff80000000 14 );
  (TEST_IMM_OP 6  'srliw #x0000000000000001 #xffffffff80000001 31 );

  (TEST_IMM_OP 7  'srliw #xffffffffffffffff #xffffffffffffffff 0  );
  (TEST_IMM_OP 8  'srliw #x000000007fffffff #xffffffffffffffff 1  );
  (TEST_IMM_OP 9  'srliw #x0000000001ffffff #xffffffffffffffff 7  );
  (TEST_IMM_OP 10 'srliw #x000000000003ffff #xffffffffffffffff 14 );
  (TEST_IMM_OP 11 'srliw #x0000000000000001 #xffffffffffffffff 31 );

  (TEST_IMM_OP 12 'srliw #x0000000021212121 #x0000000021212121 0  );
  (TEST_IMM_OP 13 'srliw #x0000000010909090 #x0000000021212121 1  );
  (TEST_IMM_OP 14 'srliw #x0000000000424242 #x0000000021212121 7  );
  (TEST_IMM_OP 15 'srliw #x0000000000008484 #x0000000021212121 14 );
  (TEST_IMM_OP 16 'srliw #x0000000000000000 #x0000000021212121 31 );

  ; Verify that shifts ignore top 32 (using true 64-bit values)

  (TEST_IMM_OP 44 'srliw #x0000000012345678 #xffffffff12345678 0 );
  (TEST_IMM_OP 45 'srliw #x0000000001234567 #xffffffff12345678 4 );
  (TEST_IMM_OP 46 'srliw #xffffffff92345678 #x0000000092345678 0 );
  (TEST_IMM_OP 47 'srliw #x0000000009234567 #x0000000092345678 4 );

  ;-------------------------------------------------------------
  ; Source/Destination tests
  ;-------------------------------------------------------------

  (TEST_IMM_SRC1_EQ_DEST 17 'srliw #x0000000001000000 #xffffffff80000000 7 );

  ;-------------------------------------------------------------
  ; Bypassing tests
  ;-------------------------------------------------------------

  (TEST_IMM_DEST_BYPASS 18 0 'srliw #x0000000001000000 #xffffffff80000000 7  );
  (TEST_IMM_DEST_BYPASS 19 1 'srliw #x0000000000020000 #xffffffff80000000 14 );
  (TEST_IMM_DEST_BYPASS 20 2 'srliw #x0000000000000001 #xffffffff80000001 31 );

  (TEST_IMM_SRC1_BYPASS 21 0 'srliw #x0000000001000000 #xffffffff80000000 7  );
  (TEST_IMM_SRC1_BYPASS 22 1 'srliw #x0000000000020000 #xffffffff80000000 14 );
  (TEST_IMM_SRC1_BYPASS 23 2 'srliw #x0000000000000001 #xffffffff80000001 31 );

  (TEST_IMM_ZEROSRC1 24 'srliw 0 31 );
  (TEST_IMM_ZERODEST 25 'srliw 31 28 );

  



  


  



)