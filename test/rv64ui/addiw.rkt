#lang rosette
(require "../riscv/test-macros.rkt")
(provide run-riscv-addiw-tests)
(define (run-riscv-addiw-tests)
; See LICENSE for license details.

;*****************************************************************************
; 'addiw.S
;-----------------------------------------------------------------------------
;
; Test 'addiw instruction.
;

;include "riscv_test.h"
;include "test_macros.h"




  ;-------------------------------------------------------------
  ; Arithmetic tests
  ;-------------------------------------------------------------

  (TEST_IMM_OP 2  'addiw #x00000000 #x00000000 #x000 );
  (TEST_IMM_OP 3  'addiw #x00000002 #x00000001 #x001 );
  (TEST_IMM_OP 4  'addiw #x0000000a #x00000003 #x007 );

  (TEST_IMM_OP 5  'addiw #xfffffffffffff800 #x0000000000000000 #x800 );
  (TEST_IMM_OP 6  'addiw #xffffffff80000000 #xffffffff80000000 #x000 );
  (TEST_IMM_OP 7  'addiw #x000000007ffff800 #xffffffff80000000 #x800 );

  (TEST_IMM_OP 8  'addiw #x00000000000007ff #x00000000 #x7ff );
  (TEST_IMM_OP 9  'addiw #x000000007fffffff #x7fffffff #x000 );
  (TEST_IMM_OP 10 'addiw #xffffffff800007fe #x7fffffff #x7ff );

  (TEST_IMM_OP 11 'addiw #xffffffff800007ff #xffffffff80000000 #x7ff );
  (TEST_IMM_OP 12 'addiw #x000000007ffff7ff #x000000007fffffff #x800 );

  (TEST_IMM_OP 13 'addiw #xffffffffffffffff #x0000000000000000 #xfff );
  (TEST_IMM_OP 14 'addiw #x0000000000000000 #xffffffffffffffff #x001 );
  (TEST_IMM_OP 15 'addiw #xfffffffffffffffe #xffffffffffffffff #xfff );

  (TEST_IMM_OP 16 'addiw #xffffffff80000000 #x7fffffff #x001 );

  ;-------------------------------------------------------------
  ; Source/Destination tests
  ;-------------------------------------------------------------

  (TEST_IMM_SRC1_EQ_DEST 17 'addiw 24 13 11 );

  ;-------------------------------------------------------------
  ; Bypassing tests
  ;-------------------------------------------------------------

  (TEST_IMM_DEST_BYPASS 18 0 'addiw 24 13 11 );
  (TEST_IMM_DEST_BYPASS 19 1 'addiw 23 13 10 );
  (TEST_IMM_DEST_BYPASS 20 2 'addiw 22 13  9 );

  (TEST_IMM_SRC1_BYPASS 21 0 'addiw 24 13 11 );
  (TEST_IMM_SRC1_BYPASS 22 1 'addiw 23 13 10 );
  (TEST_IMM_SRC1_BYPASS 23 2 'addiw 22 13  9 );

  (TEST_IMM_ZEROSRC1 24 'addiw 32 32 );
  (TEST_IMM_ZERODEST 25 'addiw 33 50 );

  



  


  



)