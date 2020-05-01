#lang rosette
(require "../riscv/test-macros.rkt")
(provide run-riscv-slli-tests)
(define (run-riscv-slli-tests)
; See LICENSE for license details.

;*****************************************************************************
; 'slli.S
;-----------------------------------------------------------------------------
;
; Test 'slli instruction.
;

;include "riscv_test.h"
;include "test_macros.h"




  ;-------------------------------------------------------------
  ; Arithmetic tests
  ;-------------------------------------------------------------

  (TEST_IMM_OP 2  'slli #x0000000000000001 #x0000000000000001 0  );
  (TEST_IMM_OP 3  'slli #x0000000000000002 #x0000000000000001 1  );
  (TEST_IMM_OP 4  'slli #x0000000000000080 #x0000000000000001 7  );
  (TEST_IMM_OP 5  'slli #x0000000000004000 #x0000000000000001 14 );
  (TEST_IMM_OP 6  'slli #x0000000080000000 #x0000000000000001 31 );

  (TEST_IMM_OP 7  'slli #xffffffffffffffff #xffffffffffffffff 0  );
  (TEST_IMM_OP 8  'slli #xfffffffffffffffe #xffffffffffffffff 1  );
  (TEST_IMM_OP 9  'slli #xffffffffffffff80 #xffffffffffffffff 7  );
  (TEST_IMM_OP 10 'slli #xffffffffffffc000 #xffffffffffffffff 14 );
  (TEST_IMM_OP 11 'slli #xffffffff80000000 #xffffffffffffffff 31 );

  (TEST_IMM_OP 12 'slli #x0000000021212121 #x0000000021212121 0  );
  (TEST_IMM_OP 13 'slli #x0000000042424242 #x0000000021212121 1  );
  (TEST_IMM_OP 14 'slli #x0000001090909080 #x0000000021212121 7  );
  (TEST_IMM_OP 15 'slli #x0000084848484000 #x0000000021212121 14 );
  (TEST_IMM_OP 16 'slli #x1090909080000000 #x0000000021212121 31 );

;if __riscv_xlen == 64
  (TEST_IMM_OP 50 'slli #x8000000000000000 #x0000000000000001 63 );
  (TEST_IMM_OP 51 'slli #xffffff8000000000 #xffffffffffffffff 39 );
  (TEST_IMM_OP 52 'slli #x0909080000000000 #x0000000021212121 43 );
;endif

  ;-------------------------------------------------------------
  ; Source/Destination tests
  ;-------------------------------------------------------------

  (TEST_IMM_SRC1_EQ_DEST 17 'slli #x00000080 #x00000001 7 );

  ;-------------------------------------------------------------
  ; Bypassing tests
  ;-------------------------------------------------------------

  (TEST_IMM_DEST_BYPASS 18 0 'slli #x0000000000000080 #x0000000000000001 7  );
  (TEST_IMM_DEST_BYPASS 19 1 'slli #x0000000000004000 #x0000000000000001 14 );
  (TEST_IMM_DEST_BYPASS 20 2 'slli #x0000000080000000 #x0000000000000001 31 );

  (TEST_IMM_SRC1_BYPASS 21 0 'slli #x0000000000000080 #x0000000000000001 7  );
  (TEST_IMM_SRC1_BYPASS 22 1 'slli #x0000000000004000 #x0000000000000001 14 );
  (TEST_IMM_SRC1_BYPASS 23 2 'slli #x0000000080000000 #x0000000000000001 31 );

  (TEST_IMM_ZEROSRC1 24 'slli 0 31 );
  (TEST_IMM_ZERODEST 25 'slli 33 20 );

  



  


  



)