#lang rosette
(require "../riscv/test-macros.rkt")
(provide run-riscv-srlw-tests)
(define (run-riscv-srlw-tests)
; See LICENSE for license details.

;*****************************************************************************
; 'srlw.S
;-----------------------------------------------------------------------------
;
; Test 'srlw instruction.
;

;include "riscv_test.h"
;include "test_macros.h"




  ;-------------------------------------------------------------
  ; Arithmetic tests
  ;-------------------------------------------------------------

  (TEST_RR_OP 2  'srlw #xffffffff80000000 #xffffffff80000000 0  );
  (TEST_RR_OP 3  'srlw #x0000000040000000 #xffffffff80000000 1  );
  (TEST_RR_OP 4  'srlw #x0000000001000000 #xffffffff80000000 7  );
  (TEST_RR_OP 5  'srlw #x0000000000020000 #xffffffff80000000 14 );
  (TEST_RR_OP 6  'srlw #x0000000000000001 #xffffffff80000001 31 );

  (TEST_RR_OP 7  'srlw #xffffffffffffffff #xffffffffffffffff 0  );
  (TEST_RR_OP 8  'srlw #x000000007fffffff #xffffffffffffffff 1  );
  (TEST_RR_OP 9  'srlw #x0000000001ffffff #xffffffffffffffff 7  );
  (TEST_RR_OP 10 'srlw #x000000000003ffff #xffffffffffffffff 14 );
  (TEST_RR_OP 11 'srlw #x0000000000000001 #xffffffffffffffff 31 );

  (TEST_RR_OP 12 'srlw #x0000000021212121 #x0000000021212121 0  );
  (TEST_RR_OP 13 'srlw #x0000000010909090 #x0000000021212121 1  );
  (TEST_RR_OP 14 'srlw #x0000000000424242 #x0000000021212121 7  );
  (TEST_RR_OP 15 'srlw #x0000000000008484 #x0000000021212121 14 );
  (TEST_RR_OP 16 'srlw #x0000000000000000 #x0000000021212121 31 );

  ; Verify that shifts only use bottom five bits

  (TEST_RR_OP 17 'srlw #x0000000021212121 #x0000000021212121 #xffffffffffffffe0 );
  (TEST_RR_OP 18 'srlw #x0000000010909090 #x0000000021212121 #xffffffffffffffe1 );
  (TEST_RR_OP 19 'srlw #x0000000000424242 #x0000000021212121 #xffffffffffffffe7 );
  (TEST_RR_OP 20 'srlw #x0000000000008484 #x0000000021212121 #xffffffffffffffee );
  (TEST_RR_OP 21 'srlw #x0000000000000000 #x0000000021212121 #xffffffffffffffff );

  ; Verify that shifts ignore top 32 (using true 64-bit values)

  (TEST_RR_OP 44 'srlw #x0000000012345678 #xffffffff12345678 0 );
  (TEST_RR_OP 45 'srlw #x0000000001234567 #xffffffff12345678 4 );
  (TEST_RR_OP 46 'srlw #xffffffff92345678 #x0000000092345678 0 );
  (TEST_RR_OP 47 'srlw #x0000000009234567 #x0000000092345678 4 );

  ;-------------------------------------------------------------
  ; Source/Destination tests
  ;-------------------------------------------------------------

  (TEST_RR_SRC1_EQ_DEST 22 'srlw #x0000000001000000 #xffffffff80000000 7  );
  (TEST_RR_SRC2_EQ_DEST 23 'srlw #x0000000000020000 #xffffffff80000000 14 );
  (TEST_RR_SRC12_EQ_DEST 24 'srlw 0 7 );

  ;-------------------------------------------------------------
  ; Bypassing tests
  ;-------------------------------------------------------------

  (TEST_RR_DEST_BYPASS 25 0 'srlw #x0000000001000000 #xffffffff80000000 7  );
  (TEST_RR_DEST_BYPASS 26 1 'srlw #x0000000000020000 #xffffffff80000000 14 );
  (TEST_RR_DEST_BYPASS 27 2 'srlw #x0000000000000001 #xffffffff80000000 31 );

  (TEST_RR_SRC12_BYPASS 28 0 0 'srlw #x0000000001000000 #xffffffff80000000 7  );
  (TEST_RR_SRC12_BYPASS 29 0 1 'srlw #x0000000000020000 #xffffffff80000000 14 );
  (TEST_RR_SRC12_BYPASS 30 0 2 'srlw #x0000000000000001 #xffffffff80000000 31 );
  (TEST_RR_SRC12_BYPASS 31 1 0 'srlw #x0000000001000000 #xffffffff80000000 7  );
  (TEST_RR_SRC12_BYPASS 32 1 1 'srlw #x0000000000020000 #xffffffff80000000 14 );
  (TEST_RR_SRC12_BYPASS 33 2 0 'srlw #x0000000000000001 #xffffffff80000000 31 );

  (TEST_RR_SRC21_BYPASS 34 0 0 'srlw #x0000000001000000 #xffffffff80000000 7  );
  (TEST_RR_SRC21_BYPASS 35 0 1 'srlw #x0000000000020000 #xffffffff80000000 14 );
  (TEST_RR_SRC21_BYPASS 36 0 2 'srlw #x0000000000000001 #xffffffff80000000 31 );
  (TEST_RR_SRC21_BYPASS 37 1 0 'srlw #x0000000001000000 #xffffffff80000000 7  );
  (TEST_RR_SRC21_BYPASS 38 1 1 'srlw #x0000000000020000 #xffffffff80000000 14 );
  (TEST_RR_SRC21_BYPASS 39 2 0 'srlw #x0000000000000001 #xffffffff80000000 31 );

  (TEST_RR_ZEROSRC1 40 'srlw 0 15 );
  (TEST_RR_ZEROSRC2 41 'srlw 32 32 );
  (TEST_RR_ZEROSRC12 42 'srlw 0 );
  (TEST_RR_ZERODEST 43 'srlw 1024 2048 );

  



  


  



)