#lang rosette
(require "../riscv/test-macros.rkt")
(provide run-riscv-srl-tests)
(define (run-riscv-srl-tests)
; See LICENSE for license details.

;*****************************************************************************
; 'srl.S
;-----------------------------------------------------------------------------
;
; Test 'srl instruction.
;

;include "riscv_test.h"
;include "test_macros.h"




  ;-------------------------------------------------------------
  ; Arithmetic tests
  ;-------------------------------------------------------------

  (TEST_SRL 2  #xffffffff80000000 0  );
  (TEST_SRL 3  #xffffffff80000000 1  );
  (TEST_SRL 4  #xffffffff80000000 7  );
  (TEST_SRL 5  #xffffffff80000000 14 );
  (TEST_SRL 6  #xffffffff80000001 31 );

  (TEST_SRL 7  #xffffffffffffffff 0  );
  (TEST_SRL 8  #xffffffffffffffff 1  );
  (TEST_SRL 9  #xffffffffffffffff 7  );
  (TEST_SRL 10 #xffffffffffffffff 14 );
  (TEST_SRL 11 #xffffffffffffffff 31 );

  (TEST_SRL 12 #x0000000021212121 0  );
  (TEST_SRL 13 #x0000000021212121 1  );
  (TEST_SRL 14 #x0000000021212121 7  );
  (TEST_SRL 15 #x0000000021212121 14 );
  (TEST_SRL 16 #x0000000021212121 31 );

  ; Verify that shifts only use bottom six(rv64) or five(rv32) bits

  (TEST_RR_OP 17 'srl #x0000000021212121 #x0000000021212121 #xffffffffffffffc0 );
  (TEST_RR_OP 18 'srl #x0000000010909090 #x0000000021212121 #xffffffffffffffc1 );
  (TEST_RR_OP 19 'srl #x0000000000424242 #x0000000021212121 #xffffffffffffffc7 );
  (TEST_RR_OP 20 'srl #x0000000000008484 #x0000000021212121 #xffffffffffffffce );
  (TEST_RR_OP 21 'srl #x0000000000000000 #x0000000021212121 #xffffffffffffffff );

  ;-------------------------------------------------------------
  ; Source/Destination tests
  ;-------------------------------------------------------------

  (TEST_RR_SRC1_EQ_DEST 22 'srl #x01000000 #x80000000 7  );
  (TEST_RR_SRC2_EQ_DEST 23 'srl #x00020000 #x80000000 14 );
  (TEST_RR_SRC12_EQ_DEST 24 'srl 0 7 );

  ;-------------------------------------------------------------
  ; Bypassing tests
  ;-------------------------------------------------------------

  (TEST_RR_DEST_BYPASS 25 0 'srl #x01000000 #x80000000 7  );
  (TEST_RR_DEST_BYPASS 26 1 'srl #x00020000 #x80000000 14 );
  (TEST_RR_DEST_BYPASS 27 2 'srl #x00000001 #x80000000 31 );

  (TEST_RR_SRC12_BYPASS 28 0 0 'srl #x01000000 #x80000000 7  );
  (TEST_RR_SRC12_BYPASS 29 0 1 'srl #x00020000 #x80000000 14 );
  (TEST_RR_SRC12_BYPASS 30 0 2 'srl #x00000001 #x80000000 31 );
  (TEST_RR_SRC12_BYPASS 31 1 0 'srl #x01000000 #x80000000 7  );
  (TEST_RR_SRC12_BYPASS 32 1 1 'srl #x00020000 #x80000000 14 );
  (TEST_RR_SRC12_BYPASS 33 2 0 'srl #x00000001 #x80000000 31 );

  (TEST_RR_SRC21_BYPASS 34 0 0 'srl #x01000000 #x80000000 7  );
  (TEST_RR_SRC21_BYPASS 35 0 1 'srl #x00020000 #x80000000 14 );
  (TEST_RR_SRC21_BYPASS 36 0 2 'srl #x00000001 #x80000000 31 );
  (TEST_RR_SRC21_BYPASS 37 1 0 'srl #x01000000 #x80000000 7  );
  (TEST_RR_SRC21_BYPASS 38 1 1 'srl #x00020000 #x80000000 14 );
  (TEST_RR_SRC21_BYPASS 39 2 0 'srl #x00000001 #x80000000 31 );

  (TEST_RR_ZEROSRC1 40 'srl 0 15 );
  (TEST_RR_ZEROSRC2 41 'srl 32 32 );
  (TEST_RR_ZEROSRC12 42 'srl 0 );
  (TEST_RR_ZERODEST 43 'srl 1024 2048 );

  



  


  



)
