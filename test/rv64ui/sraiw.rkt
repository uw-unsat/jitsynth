#lang rosette
(require "../riscv/test-macros.rkt")
(provide run-riscv-sraiw-tests)
(define (run-riscv-sraiw-tests)
; See LICENSE for license details.

;*****************************************************************************
; 'sraiw.S
;-----------------------------------------------------------------------------
;
; Test 'sraiw instruction.
;

;include "riscv_test.h"
;include "test_macros.h"




  ;-------------------------------------------------------------
  ; Arithmetic tests
  ;-------------------------------------------------------------

  (TEST_IMM_OP 2  'sraiw #xffffffff80000000 #xffffffff80000000 0  );
  (TEST_IMM_OP 3  'sraiw #xffffffffc0000000 #xffffffff80000000 1  );
  (TEST_IMM_OP 4  'sraiw #xffffffffff000000 #xffffffff80000000 7  );
  (TEST_IMM_OP 5  'sraiw #xfffffffffffe0000 #xffffffff80000000 14 );
  (TEST_IMM_OP 6  'sraiw #xffffffffffffffff #xffffffff80000001 31 );

  (TEST_IMM_OP 7  'sraiw #x000000007fffffff #x000000007fffffff 0  );
  (TEST_IMM_OP 8  'sraiw #x000000003fffffff #x000000007fffffff 1  );
  (TEST_IMM_OP 9  'sraiw #x0000000000ffffff #x000000007fffffff 7  );
  (TEST_IMM_OP 10 'sraiw #x000000000001ffff #x000000007fffffff 14 );
  (TEST_IMM_OP 11 'sraiw #x0000000000000000 #x000000007fffffff 31 );

  (TEST_IMM_OP 12 'sraiw #xffffffff81818181 #xffffffff81818181 0  );
  (TEST_IMM_OP 13 'sraiw #xffffffffc0c0c0c0 #xffffffff81818181 1  );
  (TEST_IMM_OP 14 'sraiw #xffffffffff030303 #xffffffff81818181 7  );
  (TEST_IMM_OP 15 'sraiw #xfffffffffffe0606 #xffffffff81818181 14 );
  (TEST_IMM_OP 16 'sraiw #xffffffffffffffff #xffffffff81818181 31 );

  ; Verify that shifts ignore top 32 (using true 64-bit values)

  (TEST_IMM_OP 44 'sraiw #x0000000012345678 #xffffffff12345678 0 );
  (TEST_IMM_OP 45 'sraiw #x0000000001234567 #xffffffff12345678 4 );
  (TEST_IMM_OP 46 'sraiw #xffffffff92345678 #x0000000092345678 0 );
  (TEST_IMM_OP 47 'sraiw #xfffffffff9234567 #x0000000092345678 4 );

  ;-------------------------------------------------------------
  ; Source/Destination tests
  ;-------------------------------------------------------------

  (TEST_IMM_SRC1_EQ_DEST 17 'sraiw #xffffffffff000000 #xffffffff80000000 7 );

  ;-------------------------------------------------------------
  ; Bypassing tests
  ;-------------------------------------------------------------

  (TEST_IMM_DEST_BYPASS 18 0 'sraiw #xffffffffff000000 #xffffffff80000000 7  );
  (TEST_IMM_DEST_BYPASS 19 1 'sraiw #xfffffffffffe0000 #xffffffff80000000 14 );
  (TEST_IMM_DEST_BYPASS 20 2 'sraiw #xffffffffffffffff #xffffffff80000001 31 );

  (TEST_IMM_SRC1_BYPASS 21 0 'sraiw #xffffffffff000000 #xffffffff80000000 7 );
  (TEST_IMM_SRC1_BYPASS 22 1 'sraiw #xfffffffffffe0000 #xffffffff80000000 14 );
  (TEST_IMM_SRC1_BYPASS 23 2 'sraiw #xffffffffffffffff #xffffffff80000001 31 );

  (TEST_IMM_ZEROSRC1 24 'sraiw 0 31 );
  (TEST_IMM_ZERODEST 25 'sraiw 31 28 );

  (TEST_IMM_OP 26 'sraiw #x0000000000000000 #x00e0000000000000 28)
  (TEST_IMM_OP 27 'sraiw #xffffffffff000000 #x00000000f0000000 4)

  



  


  



)