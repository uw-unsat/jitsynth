#lang rosette
(require "../riscv/test-macros.rkt")
(provide run-riscv-sra-tests)
(define (run-riscv-sra-tests)
; See LICENSE for license details.

;*****************************************************************************
; 'sra.S
;-----------------------------------------------------------------------------
;
; Test 'sra instruction.
;

;include "riscv_test.h"
;include "test_macros.h"




  ;-------------------------------------------------------------
  ; Arithmetic tests
  ;-------------------------------------------------------------

  (TEST_RR_OP 2  'sra #xffffffff80000000 #xffffffff80000000 0  );
  (TEST_RR_OP 3  'sra #xffffffffc0000000 #xffffffff80000000 1  );
  (TEST_RR_OP 4  'sra #xffffffffff000000 #xffffffff80000000 7  );
  (TEST_RR_OP 5  'sra #xfffffffffffe0000 #xffffffff80000000 14 );
  (TEST_RR_OP 6  'sra #xffffffffffffffff #xffffffff80000001 31 );

  (TEST_RR_OP 7  'sra #x000000007fffffff #x000000007fffffff 0  );
  (TEST_RR_OP 8  'sra #x000000003fffffff #x000000007fffffff 1  );
  (TEST_RR_OP 9  'sra #x0000000000ffffff #x000000007fffffff 7  );
  (TEST_RR_OP 10 'sra #x000000000001ffff #x000000007fffffff 14 );
  (TEST_RR_OP 11 'sra #x0000000000000000 #x000000007fffffff 31 );

  (TEST_RR_OP 12 'sra #xffffffff81818181 #xffffffff81818181 0  );
  (TEST_RR_OP 13 'sra #xffffffffc0c0c0c0 #xffffffff81818181 1  );
  (TEST_RR_OP 14 'sra #xffffffffff030303 #xffffffff81818181 7  );
  (TEST_RR_OP 15 'sra #xfffffffffffe0606 #xffffffff81818181 14 );
  (TEST_RR_OP 16 'sra #xffffffffffffffff #xffffffff81818181 31 );

  ; Verify that shifts only use bottom six(rv64) or five(rv32) bits

  (TEST_RR_OP 17 'sra #xffffffff81818181 #xffffffff81818181 #xffffffffffffffc0 );
  (TEST_RR_OP 18 'sra #xffffffffc0c0c0c0 #xffffffff81818181 #xffffffffffffffc1 );
  (TEST_RR_OP 19 'sra #xffffffffff030303 #xffffffff81818181 #xffffffffffffffc7 );
  (TEST_RR_OP 20 'sra #xfffffffffffe0606 #xffffffff81818181 #xffffffffffffffce );
  (TEST_RR_OP 21 'sra #xffffffffffffffff #xffffffff81818181 #xffffffffffffffff );

  ;-------------------------------------------------------------
  ; Source/Destination tests
  ;-------------------------------------------------------------

  (TEST_RR_SRC1_EQ_DEST 22 'sra #xffffffffff000000 #xffffffff80000000 7  );
  (TEST_RR_SRC2_EQ_DEST 23 'sra #xfffffffffffe0000 #xffffffff80000000 14 );
  (TEST_RR_SRC12_EQ_DEST 24 'sra 0 7 );

  ;-------------------------------------------------------------
  ; Bypassing tests
  ;-------------------------------------------------------------

  (TEST_RR_DEST_BYPASS 25 0 'sra #xffffffffff000000 #xffffffff80000000 7  );
  (TEST_RR_DEST_BYPASS 26 1 'sra #xfffffffffffe0000 #xffffffff80000000 14 );
  (TEST_RR_DEST_BYPASS 27 2 'sra #xffffffffffffffff #xffffffff80000000 31 );

  (TEST_RR_SRC12_BYPASS 28 0 0 'sra #xffffffffff000000 #xffffffff80000000 7  );
  (TEST_RR_SRC12_BYPASS 29 0 1 'sra #xfffffffffffe0000 #xffffffff80000000 14 );
  (TEST_RR_SRC12_BYPASS 30 0 2 'sra #xffffffffffffffff #xffffffff80000000 31 );
  (TEST_RR_SRC12_BYPASS 31 1 0 'sra #xffffffffff000000 #xffffffff80000000 7  );
  (TEST_RR_SRC12_BYPASS 32 1 1 'sra #xfffffffffffe0000 #xffffffff80000000 14 );
  (TEST_RR_SRC12_BYPASS 33 2 0 'sra #xffffffffffffffff #xffffffff80000000 31 );

  (TEST_RR_SRC21_BYPASS 34 0 0 'sra #xffffffffff000000 #xffffffff80000000 7  );
  (TEST_RR_SRC21_BYPASS 35 0 1 'sra #xfffffffffffe0000 #xffffffff80000000 14 );
  (TEST_RR_SRC21_BYPASS 36 0 2 'sra #xffffffffffffffff #xffffffff80000000 31 );
  (TEST_RR_SRC21_BYPASS 37 1 0 'sra #xffffffffff000000 #xffffffff80000000 7  );
  (TEST_RR_SRC21_BYPASS 38 1 1 'sra #xfffffffffffe0000 #xffffffff80000000 14 );
  (TEST_RR_SRC21_BYPASS 39 2 0 'sra #xffffffffffffffff #xffffffff80000000 31 );

  (TEST_RR_ZEROSRC1 40 'sra 0 15 );
  (TEST_RR_ZEROSRC2 41 'sra 32 32 );
  (TEST_RR_ZEROSRC12 42 'sra 0 );
  (TEST_RR_ZERODEST 43 'sra 1024 2048 );

  



  


  



)