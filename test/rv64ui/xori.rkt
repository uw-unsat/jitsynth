#lang rosette
(require "../riscv/test-macros.rkt")
(provide run-riscv-xori-tests)
(define (run-riscv-xori-tests)
; See LICENSE for license details.

;*****************************************************************************
; 'xori.S
;-----------------------------------------------------------------------------
;
; Test 'xori instruction.
;

;include "riscv_test.h"
;include "test_macros.h"




  ;-------------------------------------------------------------
  ; Logical tests
  ;-------------------------------------------------------------

  (TEST_IMM_OP 2 'xori #xffffffffff00f00f #x0000000000ff0f00 #xf0f );
  (TEST_IMM_OP 3 'xori #x000000000ff00f00 #x000000000ff00ff0 #x0f0 );
  (TEST_IMM_OP 4 'xori #x0000000000ff0ff0 #x0000000000ff08ff #x70f );
  (TEST_IMM_OP 5 'xori #xfffffffff00ff0ff #xfffffffff00ff00f #x0f0 );

  ;-------------------------------------------------------------
  ; Source/Destination tests
  ;-------------------------------------------------------------

  (TEST_IMM_SRC1_EQ_DEST 6 'xori #xffffffffff00f00f #xffffffffff00f700 #x70f );

   ;-------------------------------------------------------------
  ; Bypassing tests
  ;-------------------------------------------------------------

  (TEST_IMM_DEST_BYPASS 7  0 'xori #x000000000ff00f00 #x000000000ff00ff0 #x0f0 );
  (TEST_IMM_DEST_BYPASS 8  1 'xori #x0000000000ff0ff0 #x0000000000ff08ff #x70f );
  (TEST_IMM_DEST_BYPASS 9  2 'xori #xfffffffff00ff0ff #xfffffffff00ff00f #x0f0 );

  (TEST_IMM_SRC1_BYPASS 10 0 'xori #x000000000ff00f00 #x000000000ff00ff0 #x0f0 );
  (TEST_IMM_SRC1_BYPASS 11 1 'xori #x0000000000ff0ff0 #x0000000000ff0fff #x00f );
  (TEST_IMM_SRC1_BYPASS 12 2 'xori #xfffffffff00ff0ff #xfffffffff00ff00f #x0f0 );

  (TEST_IMM_ZEROSRC1 13 'xori #x0f0 #x0f0 );
  (TEST_IMM_ZERODEST 14 'xori #x00ff00ff #x70f );

  



  


  



)