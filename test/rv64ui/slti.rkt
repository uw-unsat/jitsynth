#lang rosette
(require "../riscv/test-macros.rkt")
(provide run-riscv-slti-tests)
(define (run-riscv-slti-tests)
; See LICENSE for license details.

;*****************************************************************************
; 'slti.S
;-----------------------------------------------------------------------------
;
; Test 'slti instruction.
;

;include "riscv_test.h"
;include "test_macros.h"




  ;-------------------------------------------------------------
  ; Arithmetic tests
  ;-------------------------------------------------------------

  (TEST_IMM_OP 2  'slti 0 #x0000000000000000 #x000 );
  (TEST_IMM_OP 3  'slti 0 #x0000000000000001 #x001 );
  (TEST_IMM_OP 4  'slti 1 #x0000000000000003 #x007 );
  (TEST_IMM_OP 5  'slti 0 #x0000000000000007 #x003 );

  (TEST_IMM_OP 6  'slti 0 #x0000000000000000 #x800 );
  (TEST_IMM_OP 7  'slti 1 #xffffffff80000000 #x000 );
  (TEST_IMM_OP 8  'slti 1 #xffffffff80000000 #x800 );

  (TEST_IMM_OP 9  'slti 1 #x0000000000000000 #x7ff );
  (TEST_IMM_OP 10 'slti 0 #x000000007fffffff #x000 );
  (TEST_IMM_OP 11 'slti 0 #x000000007fffffff #x7ff );

  (TEST_IMM_OP 12 'slti 1 #xffffffff80000000 #x7ff );
  (TEST_IMM_OP 13 'slti 0 #x000000007fffffff #x800 );

  (TEST_IMM_OP 14 'slti 0 #x0000000000000000 #xfff );
  (TEST_IMM_OP 15 'slti 1 #xffffffffffffffff #x001 );
  (TEST_IMM_OP 16 'slti 0 #xffffffffffffffff #xfff );

  ;-------------------------------------------------------------
  ; Source/Destination tests
  ;-------------------------------------------------------------

  (TEST_IMM_SRC1_EQ_DEST 17 'slti 1 11 13 );

  ;-------------------------------------------------------------
  ; Bypassing tests
  ;-------------------------------------------------------------

  (TEST_IMM_DEST_BYPASS 18 0 'slti 0 15 10 );
  (TEST_IMM_DEST_BYPASS 19 1 'slti 1 10 16 );
  (TEST_IMM_DEST_BYPASS 20 2 'slti 0 16  9 );

  (TEST_IMM_SRC1_BYPASS 21 0 'slti 1 11 15 );
  (TEST_IMM_SRC1_BYPASS 22 1 'slti 0 17  8 );
  (TEST_IMM_SRC1_BYPASS 23 2 'slti 1 12 14 );

  (TEST_IMM_ZEROSRC1 24 'slti 0 #xfff );
  (TEST_IMM_ZERODEST 25 'slti #x00ff00ff #xfff );

  



  


  



)