#lang rosette
(require "../riscv/test-macros.rkt")
(provide run-riscv-or-tests)
(define (run-riscv-or-tests)
; See LICENSE f'or license details.

;*****************************************************************************
; 'or.S
;-----------------------------------------------------------------------------
;
; Test 'or instruction.
;

;include "riscv_test.h"
;include "test_macros.h"




  ;-------------------------------------------------------------
  ; Logical tests
  ;-------------------------------------------------------------

  (TEST_RR_OP 2 'or #xff0fff0f #xff00ff00 #x0f0f0f0f );
  (TEST_RR_OP 3 'or #xfff0fff0 #x0ff00ff0 #xf0f0f0f0 );
  (TEST_RR_OP 4 'or #x0fff0fff #x00ff00ff #x0f0f0f0f );
  (TEST_RR_OP 5 'or #xf0fff0ff #xf00ff00f #xf0f0f0f0 );

  ;-------------------------------------------------------------
  ; Source/Destination tests
  ;-------------------------------------------------------------

  (TEST_RR_SRC1_EQ_DEST 6 'or #xff0fff0f #xff00ff00 #x0f0f0f0f );
  (TEST_RR_SRC2_EQ_DEST 7 'or #xff0fff0f #xff00ff00 #x0f0f0f0f );
  (TEST_RR_SRC12_EQ_DEST 8 'or #xff00ff00 #xff00ff00 );

  ;-------------------------------------------------------------
  ; Bypassing tests
  ;-------------------------------------------------------------

  (TEST_RR_DEST_BYPASS 9  0 'or #xff0fff0f #xff00ff00 #x0f0f0f0f );
  (TEST_RR_DEST_BYPASS 10 1 'or #xfff0fff0 #x0ff00ff0 #xf0f0f0f0 );
  (TEST_RR_DEST_BYPASS 11 2 'or #x0fff0fff #x00ff00ff #x0f0f0f0f );

  (TEST_RR_SRC12_BYPASS 12 0 0 'or #xff0fff0f #xff00ff00 #x0f0f0f0f );
  (TEST_RR_SRC12_BYPASS 13 0 1 'or #xfff0fff0 #x0ff00ff0 #xf0f0f0f0 );
  (TEST_RR_SRC12_BYPASS 14 0 2 'or #x0fff0fff #x00ff00ff #x0f0f0f0f );
  (TEST_RR_SRC12_BYPASS 15 1 0 'or #xff0fff0f #xff00ff00 #x0f0f0f0f );
  (TEST_RR_SRC12_BYPASS 16 1 1 'or #xfff0fff0 #x0ff00ff0 #xf0f0f0f0 );
  (TEST_RR_SRC12_BYPASS 17 2 0 'or #x0fff0fff #x00ff00ff #x0f0f0f0f );

  (TEST_RR_SRC21_BYPASS 18 0 0 'or #xff0fff0f #xff00ff00 #x0f0f0f0f );
  (TEST_RR_SRC21_BYPASS 19 0 1 'or #xfff0fff0 #x0ff00ff0 #xf0f0f0f0 );
  (TEST_RR_SRC21_BYPASS 20 0 2 'or #x0fff0fff #x00ff00ff #x0f0f0f0f );
  (TEST_RR_SRC21_BYPASS 21 1 0 'or #xff0fff0f #xff00ff00 #x0f0f0f0f );
  (TEST_RR_SRC21_BYPASS 22 1 1 'or #xfff0fff0 #x0ff00ff0 #xf0f0f0f0 );
  (TEST_RR_SRC21_BYPASS 23 2 0 'or #x0fff0fff #x00ff00ff #x0f0f0f0f );

  (TEST_RR_ZEROSRC1 24 'or #xff00ff00 #xff00ff00 );
  (TEST_RR_ZEROSRC2 25 'or #x00ff00ff #x00ff00ff );
  (TEST_RR_ZEROSRC12 26 'or 0 );
  (TEST_RR_ZERODEST 27 'or #x11111111 #x22222222 );

  



  


  



)