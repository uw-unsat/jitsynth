#lang rosette
(require "../riscv/test-macros.rkt")
(provide run-riscv-sllw-tests)
(define (run-riscv-sllw-tests)
; See LICENSE for license details.

;*****************************************************************************
; 'sllw.S
;-----------------------------------------------------------------------------
;
; Test 'sllw instruction.
;

;include "riscv_test.h"
;include "test_macros.h"




  ;-------------------------------------------------------------
  ; Arithmetic tests
  ;-------------------------------------------------------------

  (TEST_RR_OP 2  'sllw #x0000000000000001 #x0000000000000001 0  );
  (TEST_RR_OP 3  'sllw #x0000000000000002 #x0000000000000001 1  );
  (TEST_RR_OP 4  'sllw #x0000000000000080 #x0000000000000001 7  );
  (TEST_RR_OP 5  'sllw #x0000000000004000 #x0000000000000001 14 );
  (TEST_RR_OP 6  'sllw #xffffffff80000000 #x0000000000000001 31 );

  (TEST_RR_OP 7  'sllw #xffffffffffffffff #xffffffffffffffff 0  );
  (TEST_RR_OP 8  'sllw #xfffffffffffffffe #xffffffffffffffff 1  );
  (TEST_RR_OP 9  'sllw #xffffffffffffff80 #xffffffffffffffff 7  );
  (TEST_RR_OP 10 'sllw #xffffffffffffc000 #xffffffffffffffff 14 );
  (TEST_RR_OP 11 'sllw #xffffffff80000000 #xffffffffffffffff 31 );

  (TEST_RR_OP 12 'sllw #x0000000021212121 #x0000000021212121 0  );
  (TEST_RR_OP 13 'sllw #x0000000042424242 #x0000000021212121 1  );
  (TEST_RR_OP 14 'sllw #xffffffff90909080 #x0000000021212121 7  );
  (TEST_RR_OP 15 'sllw #x0000000048484000 #x0000000021212121 14 );
  (TEST_RR_OP 16 'sllw #xffffffff80000000 #x0000000021212121 31 );

  ; Verify that shifts only use bottom five bits

  (TEST_RR_OP 17 'sllw #x0000000021212121 #x0000000021212121 #xffffffffffffffe0 );
  (TEST_RR_OP 18 'sllw #x0000000042424242 #x0000000021212121 #xffffffffffffffe1 );
  (TEST_RR_OP 19 'sllw #xffffffff90909080 #x0000000021212121 #xffffffffffffffe7 );
  (TEST_RR_OP 20 'sllw #x0000000048484000 #x0000000021212121 #xffffffffffffffee );
  (TEST_RR_OP 21 'sllw #xffffffff80000000 #x0000000021212121 #xffffffffffffffff );

  ; Verify that shifts ignore top 32 (using true 64-bit values)

  (TEST_RR_OP 44 'sllw #x0000000012345678 #xffffffff12345678 0 );
  (TEST_RR_OP 45 'sllw #x0000000023456780 #xffffffff12345678 4 );
  (TEST_RR_OP 46 'sllw #xffffffff92345678 #x0000000092345678 0 );
  (TEST_RR_OP 47 'sllw #xffffffff93456780 #x0000000099345678 4 );

  ;-------------------------------------------------------------
  ; Source/Destination tests
  ;-------------------------------------------------------------

  (TEST_RR_SRC1_EQ_DEST 22 'sllw #x00000080 #x00000001 7  );
  (TEST_RR_SRC2_EQ_DEST 23 'sllw #x00004000 #x00000001 14 );
  (TEST_RR_SRC12_EQ_DEST 24 'sllw 24 3 );

  ;-------------------------------------------------------------
  ; Bypassing tests
  ;-------------------------------------------------------------

  (TEST_RR_DEST_BYPASS 25 0 'sllw #x0000000000000080 #x0000000000000001 7  );
  (TEST_RR_DEST_BYPASS 26 1 'sllw #x0000000000004000 #x0000000000000001 14 );
  (TEST_RR_DEST_BYPASS 27 2 'sllw #xffffffff80000000 #x0000000000000001 31 );

  (TEST_RR_SRC12_BYPASS 28 0 0 'sllw #x0000000000000080 #x0000000000000001 7  );
  (TEST_RR_SRC12_BYPASS 29 0 1 'sllw #x0000000000004000 #x0000000000000001 14 );
  (TEST_RR_SRC12_BYPASS 30 0 2 'sllw #xffffffff80000000 #x0000000000000001 31 );
  (TEST_RR_SRC12_BYPASS 31 1 0 'sllw #x0000000000000080 #x0000000000000001 7  );
  (TEST_RR_SRC12_BYPASS 32 1 1 'sllw #x0000000000004000 #x0000000000000001 14 );
  (TEST_RR_SRC12_BYPASS 33 2 0 'sllw #xffffffff80000000 #x0000000000000001 31 );

  (TEST_RR_SRC21_BYPASS 34 0 0 'sllw #x0000000000000080 #x0000000000000001 7  );
  (TEST_RR_SRC21_BYPASS 35 0 1 'sllw #x0000000000004000 #x0000000000000001 14 );
  (TEST_RR_SRC21_BYPASS 36 0 2 'sllw #xffffffff80000000 #x0000000000000001 31 );
  (TEST_RR_SRC21_BYPASS 37 1 0 'sllw #x0000000000000080 #x0000000000000001 7  );
  (TEST_RR_SRC21_BYPASS 38 1 1 'sllw #x0000000000004000 #x0000000000000001 14 );
  (TEST_RR_SRC21_BYPASS 39 2 0 'sllw #xffffffff80000000 #x0000000000000001 31 );

  (TEST_RR_ZEROSRC1 40 'sllw 0 15 );
  (TEST_RR_ZEROSRC2 41 'sllw 32 32 );
  (TEST_RR_ZEROSRC12 42 'sllw 0 );
  (TEST_RR_ZERODEST 43 'sllw 1024 2048 );

  



  


  



)