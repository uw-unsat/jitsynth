#lang rosette
(require "../riscv/test-macros.rkt")
(provide run-riscv-srai-tests)
(define (run-riscv-srai-tests)
; See LICENSE for license details.

;*****************************************************************************
; 'srai.S
;-----------------------------------------------------------------------------
;
; Test 'srai instruction.
;

;include "riscv_test.h"
;include "test_macros.h"




  ;-------------------------------------------------------------
  ; Arithmetic tests
  ;-------------------------------------------------------------

  (TEST_IMM_OP 2  'srai #xffffff8000000000 #xffffff8000000000 0  );
  (TEST_IMM_OP 3  'srai #xffffffffc0000000 #xffffffff80000000 1  );
  (TEST_IMM_OP 4  'srai #xffffffffff000000 #xffffffff80000000 7  );
  (TEST_IMM_OP 5  'srai #xfffffffffffe0000 #xffffffff80000000 14 );
  (TEST_IMM_OP 6  'srai #xffffffffffffffff #xffffffff80000001 31 );

  (TEST_IMM_OP 7  'srai #x000000007fffffff #x000000007fffffff 0  );
  (TEST_IMM_OP 8  'srai #x000000003fffffff #x000000007fffffff 1  );
  (TEST_IMM_OP 9  'srai #x0000000000ffffff #x000000007fffffff 7  );
  (TEST_IMM_OP 10 'srai #x000000000001ffff #x000000007fffffff 14 );
  (TEST_IMM_OP 11 'srai #x0000000000000000 #x000000007fffffff 31 );

  (TEST_IMM_OP 12 'srai #xffffffff81818181 #xffffffff81818181 0  );
  (TEST_IMM_OP 13 'srai #xffffffffc0c0c0c0 #xffffffff81818181 1  );
  (TEST_IMM_OP 14 'srai #xffffffffff030303 #xffffffff81818181 7  );
  (TEST_IMM_OP 15 'srai #xfffffffffffe0606 #xffffffff81818181 14 );
  (TEST_IMM_OP 16 'srai #xffffffffffffffff #xffffffff81818181 31 );

  ;-------------------------------------------------------------
  ; Source/Destination tests
  ;-------------------------------------------------------------

  (TEST_IMM_SRC1_EQ_DEST 17 'srai #xffffffffff000000 #xffffffff80000000 7 );

  ;-------------------------------------------------------------
  ; Bypassing tests
  ;-------------------------------------------------------------

  (TEST_IMM_DEST_BYPASS 18 0 'srai #xffffffffff000000 #xffffffff80000000 7  );
  (TEST_IMM_DEST_BYPASS 19 1 'srai #xfffffffffffe0000 #xffffffff80000000 14 );
  (TEST_IMM_DEST_BYPASS 20 2 'srai #xffffffffffffffff #xffffffff80000001 31 );

  (TEST_IMM_SRC1_BYPASS 21 0 'srai #xffffffffff000000 #xffffffff80000000 7 );
  (TEST_IMM_SRC1_BYPASS 22 1 'srai #xfffffffffffe0000 #xffffffff80000000 14 );
  (TEST_IMM_SRC1_BYPASS 23 2 'srai #xffffffffffffffff #xffffffff80000001 31 );

  (TEST_IMM_ZEROSRC1 24 'srai 0 4 );
  (TEST_IMM_ZERODEST 25 'srai 33 10 );

  



  


  



)