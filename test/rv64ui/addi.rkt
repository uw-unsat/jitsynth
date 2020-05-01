#lang rosette
(require "../riscv/test-macros.rkt")
(provide run-riscv-addi-tests)
(define (run-riscv-addi-tests)
; See LICENSE for license details.

;*****************************************************************************
; 'addi.S
;-----------------------------------------------------------------------------
;
; Test 'addi instruction.
;

;include "riscv_test.h"
;include "test_macros.h"




  ;-------------------------------------------------------------
  ; Arithmetic tests
  ;-------------------------------------------------------------

  (TEST_IMM_OP 2  'addi #x00000000 #x00000000 #x000 );
  (TEST_IMM_OP 3  'addi #x00000002 #x00000001 #x001 );
  (TEST_IMM_OP 4  'addi #x0000000a #x00000003 #x007 );

  (TEST_IMM_OP 5  'addi #xfffffffffffff800 #x0000000000000000 #x800 );
  (TEST_IMM_OP 6  'addi #xffffffff80000000 #xffffffff80000000 #x000 );
  (TEST_IMM_OP 7  'addi #xffffffff7ffff800 #xffffffff80000000 #x800 );

  (TEST_IMM_OP 8  'addi #x00000000000007ff #x00000000 #x7ff );
  (TEST_IMM_OP 9  'addi #x000000007fffffff #x7fffffff #x000 );
  (TEST_IMM_OP 10 'addi #x00000000800007fe #x7fffffff #x7ff );

  (TEST_IMM_OP 11 'addi #xffffffff800007ff #xffffffff80000000 #x7ff );
  (TEST_IMM_OP 12 'addi #x000000007ffff7ff #x000000007fffffff #x800 );

  (TEST_IMM_OP 13 'addi #xffffffffffffffff #x0000000000000000 #xfff );
  (TEST_IMM_OP 14 'addi #x0000000000000000 #xffffffffffffffff #x001 );
  (TEST_IMM_OP 15 'addi #xfffffffffffffffe #xffffffffffffffff #xfff );

  (TEST_IMM_OP 16 'addi #x0000000080000000 #x7fffffff #x001 );

  ;-------------------------------------------------------------
  ; Source/Destination tests
  ;-------------------------------------------------------------

  (TEST_IMM_SRC1_EQ_DEST 17 'addi 24 13 11 );

  ;-------------------------------------------------------------
  ; Bypassing tests
  ;-------------------------------------------------------------

  (TEST_IMM_DEST_BYPASS 18 0 'addi 24 13 11 );
  (TEST_IMM_DEST_BYPASS 19 1 'addi 23 13 10 );
  (TEST_IMM_DEST_BYPASS 20 2 'addi 22 13  9 );

  (TEST_IMM_SRC1_BYPASS 21 0 'addi 24 13 11 );
  (TEST_IMM_SRC1_BYPASS 22 1 'addi 23 13 10 );
  (TEST_IMM_SRC1_BYPASS 23 2 'addi 22 13  9 );

  (TEST_IMM_ZEROSRC1 24 'addi 32 32 );
  (TEST_IMM_ZERODEST 25 'addi 33 50 );

  



  


  



)