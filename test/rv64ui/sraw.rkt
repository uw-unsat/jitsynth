#lang rosette
(require "../riscv/test-macros.rkt")
(provide run-riscv-sraw-tests)
(define (run-riscv-sraw-tests)
; See LICENSE for license details.

;*****************************************************************************
; 'sraw.S
;-----------------------------------------------------------------------------
;
; Test 'sraw instruction.
;

;include "riscv_test.h"
;include "test_macros.h"




  ;-------------------------------------------------------------
  ; Arithmetic tests
  ;-------------------------------------------------------------

  (TEST_RR_OP 2  'sraw #xffffffff80000000 #xffffffff80000000 0  );
  (TEST_RR_OP 3  'sraw #xffffffffc0000000 #xffffffff80000000 1  );
  (TEST_RR_OP 4  'sraw #xffffffffff000000 #xffffffff80000000 7  );
  (TEST_RR_OP 5  'sraw #xfffffffffffe0000 #xffffffff80000000 14 );
  (TEST_RR_OP 6  'sraw #xffffffffffffffff #xffffffff80000001 31 );

  (TEST_RR_OP 7  'sraw #x000000007fffffff #x000000007fffffff 0  );
  (TEST_RR_OP 8  'sraw #x000000003fffffff #x000000007fffffff 1  );
  (TEST_RR_OP 9  'sraw #x0000000000ffffff #x000000007fffffff 7  );
  (TEST_RR_OP 10 'sraw #x000000000001ffff #x000000007fffffff 14 );
  (TEST_RR_OP 11 'sraw #x0000000000000000 #x000000007fffffff 31 );

  (TEST_RR_OP 12 'sraw #xffffffff81818181 #xffffffff81818181 0  );
  (TEST_RR_OP 13 'sraw #xffffffffc0c0c0c0 #xffffffff81818181 1  );
  (TEST_RR_OP 14 'sraw #xffffffffff030303 #xffffffff81818181 7  );
  (TEST_RR_OP 15 'sraw #xfffffffffffe0606 #xffffffff81818181 14 );
  (TEST_RR_OP 16 'sraw #xffffffffffffffff #xffffffff81818181 31 );

  ; Verify that shifts only use bottom five bits

  (TEST_RR_OP 17 'sraw #xffffffff81818181 #xffffffff81818181 #xffffffffffffffe0 );
  (TEST_RR_OP 18 'sraw #xffffffffc0c0c0c0 #xffffffff81818181 #xffffffffffffffe1 );
  (TEST_RR_OP 19 'sraw #xffffffffff030303 #xffffffff81818181 #xffffffffffffffe7 );
  (TEST_RR_OP 20 'sraw #xfffffffffffe0606 #xffffffff81818181 #xffffffffffffffee );
  (TEST_RR_OP 21 'sraw #xffffffffffffffff #xffffffff81818181 #xffffffffffffffff );

  ; Verify that shifts ignore top 32 (using true 64-bit values)

  (TEST_RR_OP 44 'sraw #x0000000012345678 #xffffffff12345678 0 );
  (TEST_RR_OP 45 'sraw #x0000000001234567 #xffffffff12345678 4 );
  (TEST_RR_OP 46 'sraw #xffffffff92345678 #x0000000092345678 0 );
  (TEST_RR_OP 47 'sraw #xfffffffff9234567 #x0000000092345678 4 );

  ;-------------------------------------------------------------
  ; Source/Destination tests
  ;-------------------------------------------------------------

  (TEST_RR_SRC1_EQ_DEST 22 'sraw #xffffffffff000000 #xffffffff80000000 7  );
  (TEST_RR_SRC2_EQ_DEST 23 'sraw #xfffffffffffe0000 #xffffffff80000000 14 );
  (TEST_RR_SRC12_EQ_DEST 24 'sraw 0 7 );

  ;-------------------------------------------------------------
  ; Bypassing tests
  ;-------------------------------------------------------------

  (TEST_RR_DEST_BYPASS 25 0 'sraw #xffffffffff000000 #xffffffff80000000 7  );
  (TEST_RR_DEST_BYPASS 26 1 'sraw #xfffffffffffe0000 #xffffffff80000000 14 );
  (TEST_RR_DEST_BYPASS 27 2 'sraw #xffffffffffffffff #xffffffff80000000 31 );

  (TEST_RR_SRC12_BYPASS 28 0 0 'sraw #xffffffffff000000 #xffffffff80000000 7  );
  (TEST_RR_SRC12_BYPASS 29 0 1 'sraw #xfffffffffffe0000 #xffffffff80000000 14 );
  (TEST_RR_SRC12_BYPASS 30 0 2 'sraw #xffffffffffffffff #xffffffff80000000 31 );
  (TEST_RR_SRC12_BYPASS 31 1 0 'sraw #xffffffffff000000 #xffffffff80000000 7  );
  (TEST_RR_SRC12_BYPASS 32 1 1 'sraw #xfffffffffffe0000 #xffffffff80000000 14 );
  (TEST_RR_SRC12_BYPASS 33 2 0 'sraw #xffffffffffffffff #xffffffff80000000 31 );

  (TEST_RR_SRC21_BYPASS 34 0 0 'sraw #xffffffffff000000 #xffffffff80000000 7  );
  (TEST_RR_SRC21_BYPASS 35 0 1 'sraw #xfffffffffffe0000 #xffffffff80000000 14 );
  (TEST_RR_SRC21_BYPASS 36 0 2 'sraw #xffffffffffffffff #xffffffff80000000 31 );
  (TEST_RR_SRC21_BYPASS 37 1 0 'sraw #xffffffffff000000 #xffffffff80000000 7  );
  (TEST_RR_SRC21_BYPASS 38 1 1 'sraw #xfffffffffffe0000 #xffffffff80000000 14 );
  (TEST_RR_SRC21_BYPASS 39 2 0 'sraw #xffffffffffffffff #xffffffff80000000 31 );

  (TEST_RR_ZEROSRC1 40 'sraw 0 15 );
  (TEST_RR_ZEROSRC2 41 'sraw 32 32 );
  (TEST_RR_ZEROSRC12 42 'sraw 0 );
  (TEST_RR_ZERODEST 43 'sraw 1024 2048 );

  



  


  



)