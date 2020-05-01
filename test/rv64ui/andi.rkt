#lang rosette
(require "../riscv/test-macros.rkt")
(provide run-riscv-andi-tests)
(define (run-riscv-andi-tests)
; See LICENSE for license details.

;*****************************************************************************
; 'andi.S
;-----------------------------------------------------------------------------
;
; Test 'andi instruction.
;

;include "riscv_test.h"
;include "test_macros.h"




  ;-------------------------------------------------------------
  ; Logical tests
  ;-------------------------------------------------------------

  (TEST_IMM_OP 2 'andi #xff00ff00 #xff00ff00 #xf0f );
  (TEST_IMM_OP 3 'andi #x000000f0 #x0ff00ff0 #x0f0 );
  (TEST_IMM_OP 4 'andi #x0000000f #x00ff00ff #x70f );
  (TEST_IMM_OP 5 'andi #x00000000 #xf00ff00f #x0f0 );

  ;-------------------------------------------------------------
  ; Source/Destination tests
  ;-------------------------------------------------------------

  (TEST_IMM_SRC1_EQ_DEST 6 'andi #x00000000 #xff00ff00 #x0f0 );

  ;-------------------------------------------------------------
  ; Bypassing tests
  ;-------------------------------------------------------------

  (TEST_IMM_DEST_BYPASS 7  0 'andi #x00000700 #x0ff00ff0 #x70f );
  (TEST_IMM_DEST_BYPASS 8  1 'andi #x000000f0 #x00ff00ff #x0f0 );
  (TEST_IMM_DEST_BYPASS 9  2 'andi #xf00ff00f #xf00ff00f #xf0f );

  (TEST_IMM_SRC1_BYPASS 10 0 'andi #x00000700 #x0ff00ff0 #x70f );
  (TEST_IMM_SRC1_BYPASS 11 1 'andi #x000000f0 #x00ff00ff #x0f0 );
  (TEST_IMM_SRC1_BYPASS 12 2 'andi #x0000000f #xf00ff00f #x70f );

  (TEST_IMM_ZEROSRC1 13 'andi 0 #x0f0 );
  (TEST_IMM_ZERODEST 14 'andi #x00ff00ff #x70f );

  



  


  



)