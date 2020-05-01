#lang rosette
(require "../riscv/test-macros.rkt")
(provide run-riscv-xor-tests)
(define (run-riscv-xor-tests)
; See LICENSE for license details.

;*****************************************************************************
; 'xor.S
;-----------------------------------------------------------------------------
;
; Test 'xor instruction.
;

;include "riscv_test.h"
;include "test_macros.h"




  ;-------------------------------------------------------------
  ; Logical tests
  ;-------------------------------------------------------------

  (TEST_RR_OP 2 'xor #xf00ff00f #xff00ff00 #x0f0f0f0f );
  (TEST_RR_OP 3 'xor #xff00ff00 #x0ff00ff0 #xf0f0f0f0 );
  (TEST_RR_OP 4 'xor #x0ff00ff0 #x00ff00ff #x0f0f0f0f );
  (TEST_RR_OP 5 'xor #x00ff00ff #xf00ff00f #xf0f0f0f0 );

  ;-------------------------------------------------------------
  ; Source/Destination tests
  ;-------------------------------------------------------------

  (TEST_RR_SRC1_EQ_DEST 6 'xor #xf00ff00f #xff00ff00 #x0f0f0f0f );
  (TEST_RR_SRC2_EQ_DEST 7 'xor #xf00ff00f #xff00ff00 #x0f0f0f0f );
  (TEST_RR_SRC12_EQ_DEST 8 'xor #x00000000 #xff00ff00 );

  ;-------------------------------------------------------------
  ; Bypassing tests
  ;-------------------------------------------------------------

  (TEST_RR_DEST_BYPASS 9  0 'xor #xf00ff00f #xff00ff00 #x0f0f0f0f );
  (TEST_RR_DEST_BYPASS 10 1 'xor #xff00ff00 #x0ff00ff0 #xf0f0f0f0 );
  (TEST_RR_DEST_BYPASS 11 2 'xor #x0ff00ff0 #x00ff00ff #x0f0f0f0f );

  (TEST_RR_SRC12_BYPASS 12 0 0 'xor #xf00ff00f #xff00ff00 #x0f0f0f0f );
  (TEST_RR_SRC12_BYPASS 13 0 1 'xor #xff00ff00 #x0ff00ff0 #xf0f0f0f0 );
  (TEST_RR_SRC12_BYPASS 14 0 2 'xor #x0ff00ff0 #x00ff00ff #x0f0f0f0f );
  (TEST_RR_SRC12_BYPASS 15 1 0 'xor #xf00ff00f #xff00ff00 #x0f0f0f0f );
  (TEST_RR_SRC12_BYPASS 16 1 1 'xor #xff00ff00 #x0ff00ff0 #xf0f0f0f0 );
  (TEST_RR_SRC12_BYPASS 17 2 0 'xor #x0ff00ff0 #x00ff00ff #x0f0f0f0f );

  (TEST_RR_SRC21_BYPASS 18 0 0 'xor #xf00ff00f #xff00ff00 #x0f0f0f0f );
  (TEST_RR_SRC21_BYPASS 19 0 1 'xor #xff00ff00 #x0ff00ff0 #xf0f0f0f0 );
  (TEST_RR_SRC21_BYPASS 20 0 2 'xor #x0ff00ff0 #x00ff00ff #x0f0f0f0f );
  (TEST_RR_SRC21_BYPASS 21 1 0 'xor #xf00ff00f #xff00ff00 #x0f0f0f0f );
  (TEST_RR_SRC21_BYPASS 22 1 1 'xor #xff00ff00 #x0ff00ff0 #xf0f0f0f0 );
  (TEST_RR_SRC21_BYPASS 23 2 0 'xor #x0ff00ff0 #x00ff00ff #x0f0f0f0f );

  (TEST_RR_ZEROSRC1 24 'xor #xff00ff00 #xff00ff00 );
  (TEST_RR_ZEROSRC2 25 'xor #x00ff00ff #x00ff00ff );
  (TEST_RR_ZEROSRC12 26 'xor 0 );
  (TEST_RR_ZERODEST 27 'xor #x11111111 #x22222222 );

  



  


  



)