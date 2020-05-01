#lang rosette
(require "../riscv/test-macros.rkt")
(provide run-riscv-add-tests)
(define (run-riscv-add-tests)
; See LICENSE for license details.

;*****************************************************************************
; 'add.S
;-----------------------------------------------------------------------------
;
; Test 'add instruction.
;

;include "riscv_test.h"
;include "test_macros.h"




  ;-------------------------------------------------------------
  ; Arithmetic tests
  ;-------------------------------------------------------------

  (TEST_RR_OP 2  'add #x00000000 #x00000000 #x00000000 );
  (TEST_RR_OP 3  'add #x00000002 #x00000001 #x00000001 );
  (TEST_RR_OP 4  'add #x0000000a #x00000003 #x00000007 );

  (TEST_RR_OP 5  'add #xffffffffffff8000 #x0000000000000000 #xffffffffffff8000 );
  (TEST_RR_OP 6  'add #xffffffff80000000 #xffffffff80000000 #x00000000 );
  (TEST_RR_OP 7  'add #xffffffff7fff8000 #xffffffff80000000 #xffffffffffff8000 );

  (TEST_RR_OP 8  'add #x0000000000007fff #x0000000000000000 #x0000000000007fff );
  (TEST_RR_OP 9  'add #x000000007fffffff #x000000007fffffff #x0000000000000000 );
  (TEST_RR_OP 10 'add #x0000000080007ffe #x000000007fffffff #x0000000000007fff );

  (TEST_RR_OP 11 'add #xffffffff80007fff #xffffffff80000000 #x0000000000007fff );
  (TEST_RR_OP 12 'add #x000000007fff7fff #x000000007fffffff #xffffffffffff8000 );

  (TEST_RR_OP 13 'add #xffffffffffffffff #x0000000000000000 #xffffffffffffffff );
  (TEST_RR_OP 14 'add #x0000000000000000 #xffffffffffffffff #x0000000000000001 );
  (TEST_RR_OP 15 'add #xfffffffffffffffe #xffffffffffffffff #xffffffffffffffff );

  (TEST_RR_OP 16 'add #x0000000080000000 #x0000000000000001 #x000000007fffffff );

  ;-------------------------------------------------------------
  ; Source/Destination tests
  ;-------------------------------------------------------------

  (TEST_RR_SRC1_EQ_DEST 17 'add 24 13 11 );
  (TEST_RR_SRC2_EQ_DEST 18 'add 25 14 11 );
  (TEST_RR_SRC12_EQ_DEST 19 'add 26 13 );

  ;-------------------------------------------------------------
  ; Bypassing tests
  ;-------------------------------------------------------------

  (TEST_RR_DEST_BYPASS 20 0 'add 24 13 11 );
  (TEST_RR_DEST_BYPASS 21 1 'add 25 14 11 );
  (TEST_RR_DEST_BYPASS 22 2 'add 26 15 11 );

  (TEST_RR_SRC12_BYPASS 23 0 0 'add 24 13 11 );
  (TEST_RR_SRC12_BYPASS 24 0 1 'add 25 14 11 );
  (TEST_RR_SRC12_BYPASS 25 0 2 'add 26 15 11 );
  (TEST_RR_SRC12_BYPASS 26 1 0 'add 24 13 11 );
  (TEST_RR_SRC12_BYPASS 27 1 1 'add 25 14 11 );
  (TEST_RR_SRC12_BYPASS 28 2 0 'add 26 15 11 );

  (TEST_RR_SRC21_BYPASS 29 0 0 'add 24 13 11 );
  (TEST_RR_SRC21_BYPASS 30 0 1 'add 25 14 11 );
  (TEST_RR_SRC21_BYPASS 31 0 2 'add 26 15 11 );
  (TEST_RR_SRC21_BYPASS 32 1 0 'add 24 13 11 );
  (TEST_RR_SRC21_BYPASS 33 1 1 'add 25 14 11 );
  (TEST_RR_SRC21_BYPASS 34 2 0 'add 26 15 11 );

  (TEST_RR_ZEROSRC1 35 'add 15 15 );
  (TEST_RR_ZEROSRC2 36 'add 32 32 );
  (TEST_RR_ZEROSRC12 37 'add 0 );
  (TEST_RR_ZERODEST 38 'add 16 30 );

  



  


  



)