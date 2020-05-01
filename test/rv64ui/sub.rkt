#lang rosette
(require "../riscv/test-macros.rkt")
(provide run-riscv-sub-tests)
(define (run-riscv-sub-tests)
; See LICENSE for license details.

;*****************************************************************************
; 'sub.S
;-----------------------------------------------------------------------------
;
; Test 'sub instruction.
;

;include "riscv_test.h"
;include "test_macros.h"




  ;-------------------------------------------------------------
  ; Arithmetic tests
  ;-------------------------------------------------------------

  (TEST_RR_OP 2  'sub #x0000000000000000 #x0000000000000000 #x0000000000000000 );
  (TEST_RR_OP 3  'sub #x0000000000000000 #x0000000000000001 #x0000000000000001 );
  (TEST_RR_OP 4  'sub #xfffffffffffffffc #x0000000000000003 #x0000000000000007 );

  (TEST_RR_OP 5  'sub #x0000000000008000 #x0000000000000000 #xffffffffffff8000 );
  (TEST_RR_OP 6  'sub #xffffffff80000000 #xffffffff80000000 #x0000000000000000 );
  (TEST_RR_OP 7  'sub #xffffffff80008000 #xffffffff80000000 #xffffffffffff8000 );

  (TEST_RR_OP 8  'sub #xffffffffffff8001 #x0000000000000000 #x0000000000007fff );
  (TEST_RR_OP 9  'sub #x000000007fffffff #x000000007fffffff #x0000000000000000 );
  (TEST_RR_OP 10 'sub #x000000007fff8000 #x000000007fffffff #x0000000000007fff );

  (TEST_RR_OP 11 'sub #xffffffff7fff8001 #xffffffff80000000 #x0000000000007fff );
  (TEST_RR_OP 12 'sub #x0000000080007fff #x000000007fffffff #xffffffffffff8000 );

  (TEST_RR_OP 13 'sub #x0000000000000001 #x0000000000000000 #xffffffffffffffff );
  (TEST_RR_OP 14 'sub #xfffffffffffffffe #xffffffffffffffff #x0000000000000001 );
  (TEST_RR_OP 15 'sub #x0000000000000000 #xffffffffffffffff #xffffffffffffffff );

  ;-------------------------------------------------------------
  ; Source/Destination tests
  ;-------------------------------------------------------------

  (TEST_RR_SRC1_EQ_DEST 16 'sub 2 13 11 );
  (TEST_RR_SRC2_EQ_DEST 17 'sub 3 14 11 );
  (TEST_RR_SRC12_EQ_DEST 18 'sub 0 13 );

  ;-------------------------------------------------------------
  ; Bypassing tests
  ;-------------------------------------------------------------

  (TEST_RR_DEST_BYPASS 19 0 'sub 2 13 11 );
  (TEST_RR_DEST_BYPASS 20 1 'sub 3 14 11 );
  (TEST_RR_DEST_BYPASS 21 2 'sub 4 15 11 );

  (TEST_RR_SRC12_BYPASS 22 0 0 'sub 2 13 11 );
  (TEST_RR_SRC12_BYPASS 23 0 1 'sub 3 14 11 );
  (TEST_RR_SRC12_BYPASS 24 0 2 'sub 4 15 11 );
  (TEST_RR_SRC12_BYPASS 25 1 0 'sub 2 13 11 );
  (TEST_RR_SRC12_BYPASS 26 1 1 'sub 3 14 11 );
  (TEST_RR_SRC12_BYPASS 27 2 0 'sub 4 15 11 );

  (TEST_RR_SRC21_BYPASS 28 0 0 'sub 2 13 11 );
  (TEST_RR_SRC21_BYPASS 29 0 1 'sub 3 14 11 );
  (TEST_RR_SRC21_BYPASS 30 0 2 'sub 4 15 11 );
  (TEST_RR_SRC21_BYPASS 31 1 0 'sub 2 13 11 );
  (TEST_RR_SRC21_BYPASS 32 1 1 'sub 3 14 11 );
  (TEST_RR_SRC21_BYPASS 33 2 0 'sub 4 15 11 );

  (TEST_RR_ZEROSRC1 34 'sub 15 -15 );
  (TEST_RR_ZEROSRC2 35 'sub 32 32 );
  (TEST_RR_ZEROSRC12 36 'sub 0 );
  (TEST_RR_ZERODEST 37 'sub 16 30 );

  



  


  



)