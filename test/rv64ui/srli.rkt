#lang rosette
(require "../riscv/test-macros.rkt")
(provide run-riscv-srli-tests)
(define (run-riscv-srli-tests)
; See LICENSE for license details.

;*****************************************************************************
; 'srli.S
;-----------------------------------------------------------------------------
;
; Test 'srli instruction.
;

;include "riscv_test.h"
;include "test_macros.h"




  ;-------------------------------------------------------------
  ; Arithmetic tests
  ;-------------------------------------------------------------

  (TEST_SRLI 2  #xffffffff80000000 0  );
  (TEST_SRLI 3  #xffffffff80000000 1  );
  (TEST_SRLI 4  #xffffffff80000000 7  );
  (TEST_SRLI 5  #xffffffff80000000 14 );
  (TEST_SRLI 6  #xffffffff80000001 31 );

  (TEST_SRLI 7  #xffffffffffffffff 0  );
  (TEST_SRLI 8  #xffffffffffffffff 1  );
  (TEST_SRLI 9  #xffffffffffffffff 7  );
  (TEST_SRLI 10 #xffffffffffffffff 14 );
  (TEST_SRLI 11 #xffffffffffffffff 31 );

  (TEST_SRLI 12 #x0000000021212121 0  );
  (TEST_SRLI 13 #x0000000021212121 1  );
  (TEST_SRLI 14 #x0000000021212121 7  );
  (TEST_SRLI 15 #x0000000021212121 14 );
  (TEST_SRLI 16 #x0000000021212121 31 );

  ;-------------------------------------------------------------
  ; Source/Destination tests
  ;-------------------------------------------------------------

  (TEST_IMM_SRC1_EQ_DEST 17 'srli #x01000000 #x80000000 7 );

  ;-------------------------------------------------------------
  ; Bypassing tests
  ;-------------------------------------------------------------

  (TEST_IMM_DEST_BYPASS 18 0 'srli #x01000000 #x80000000 7  );
  (TEST_IMM_DEST_BYPASS 19 1 'srli #x00020000 #x80000000 14 );
  (TEST_IMM_DEST_BYPASS 20 2 'srli #x00000001 #x80000001 31 );

  (TEST_IMM_SRC1_BYPASS 21 0 'srli #x01000000 #x80000000 7  );
  (TEST_IMM_SRC1_BYPASS 22 1 'srli #x00020000 #x80000000 14 );
  (TEST_IMM_SRC1_BYPASS 23 2 'srli #x00000001 #x80000001 31 );

  (TEST_IMM_ZEROSRC1 24 'srli 0 4 );
  (TEST_IMM_ZERODEST 25 'srli 33 10 );

  



  


  



)
