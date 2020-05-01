#lang rosette
(require "../riscv/test-macros.rkt")
(provide run-riscv-ori-tests)
(define (run-riscv-ori-tests)
; See LICENSE for license details.

;*****************************************************************************
; 'ori.S
;-----------------------------------------------------------------------------
;
; Test 'ori instruction.
;

;include "riscv_test.h"
;include "test_macros.h"




  ;-------------------------------------------------------------
  ; Logical tests
  ;-------------------------------------------------------------

  (TEST_IMM_OP 2 'ori #xffffffffffffff0f #xffffffffff00ff00 #xf0f );
  (TEST_IMM_OP 3 'ori #x000000000ff00ff0 #x000000000ff00ff0 #x0f0 );
  (TEST_IMM_OP 4 'ori #x0000000000ff07ff #x0000000000ff00ff #x70f );
  (TEST_IMM_OP 5 'ori #xfffffffff00ff0ff #xfffffffff00ff00f #x0f0 );

  ;-------------------------------------------------------------
  ; Source/Destination tests
  ;-------------------------------------------------------------

  (TEST_IMM_SRC1_EQ_DEST 6 'ori #xff00fff0 #xff00ff00 #x0f0 );

  ;-------------------------------------------------------------
  ; Bypassing tests
  ;-------------------------------------------------------------

  (TEST_IMM_DEST_BYPASS 7  0 'ori #x000000000ff00ff0 #x000000000ff00ff0 #x0f0 );
  (TEST_IMM_DEST_BYPASS 8  1 'ori #x0000000000ff07ff #x0000000000ff00ff #x70f );
  (TEST_IMM_DEST_BYPASS 9  2 'ori #xfffffffff00ff0ff #xfffffffff00ff00f #x0f0 );

  (TEST_IMM_SRC1_BYPASS 10 0 'ori #x000000000ff00ff0 #x000000000ff00ff0 #x0f0 );
  (TEST_IMM_SRC1_BYPASS 11 1 'ori #xffffffffffffffff #x0000000000ff00ff #xf0f );
  (TEST_IMM_SRC1_BYPASS 12 2 'ori #xfffffffff00ff0ff #xfffffffff00ff00f #x0f0 );

  (TEST_IMM_ZEROSRC1 13 'ori #x0f0 #x0f0 );
  (TEST_IMM_ZERODEST 14 'ori #x00ff00ff #x70f );

  



  


  



)