#lang rosette
(require "../riscv/test-macros.rkt")
(provide run-riscv-sll-tests)
(define (run-riscv-sll-tests)
; See LICENSE for license details.

;*****************************************************************************
; 'sll.S
;-----------------------------------------------------------------------------
;
; Test 'sll instruction.
;

;include "riscv_test.h"
;include "test_macros.h"




  ;-------------------------------------------------------------
  ; Arithmetic tests
  ;-------------------------------------------------------------

  (TEST_RR_OP 2  'sll #x0000000000000001 #x0000000000000001 0  );
  (TEST_RR_OP 3  'sll #x0000000000000002 #x0000000000000001 1  );
  (TEST_RR_OP 4  'sll #x0000000000000080 #x0000000000000001 7  );
  (TEST_RR_OP 5  'sll #x0000000000004000 #x0000000000000001 14 );
  (TEST_RR_OP 6  'sll #x0000000080000000 #x0000000000000001 31 );

  (TEST_RR_OP 7  'sll #xffffffffffffffff #xffffffffffffffff 0  );
  (TEST_RR_OP 8  'sll #xfffffffffffffffe #xffffffffffffffff 1  );
  (TEST_RR_OP 9  'sll #xffffffffffffff80 #xffffffffffffffff 7  );
  (TEST_RR_OP 10 'sll #xffffffffffffc000 #xffffffffffffffff 14 );
  (TEST_RR_OP 11 'sll #xffffffff80000000 #xffffffffffffffff 31 );

  (TEST_RR_OP 12 'sll #x0000000021212121 #x0000000021212121 0  );
  (TEST_RR_OP 13 'sll #x0000000042424242 #x0000000021212121 1  );
  (TEST_RR_OP 14 'sll #x0000001090909080 #x0000000021212121 7  );
  (TEST_RR_OP 15 'sll #x0000084848484000 #x0000000021212121 14 );
  (TEST_RR_OP 16 'sll #x1090909080000000 #x0000000021212121 31 );

  ; Verify that shifts only use bottom six(rv64) or five(rv32) bits

  (TEST_RR_OP 17 'sll #x0000000021212121 #x0000000021212121 #xffffffffffffffc0 );
  (TEST_RR_OP 18 'sll #x0000000042424242 #x0000000021212121 #xffffffffffffffc1 );
  (TEST_RR_OP 19 'sll #x0000001090909080 #x0000000021212121 #xffffffffffffffc7 );
  (TEST_RR_OP 20 'sll #x0000084848484000 #x0000000021212121 #xffffffffffffffce );

;if __riscv_xlen == 64
  (TEST_RR_OP 21 'sll #x8000000000000000 #x0000000021212121 #xffffffffffffffff );
  (TEST_RR_OP 50 'sll #x8000000000000000 #x0000000000000001 63 );
  (TEST_RR_OP 51 'sll #xffffff8000000000 #xffffffffffffffff 39 );
  (TEST_RR_OP 52 'sll #x0909080000000000 #x0000000021212121 43 );
;endif

  ;-------------------------------------------------------------
  ; Source/Destination tests
  ;-------------------------------------------------------------

  (TEST_RR_SRC1_EQ_DEST 22 'sll #x00000080 #x00000001 7  );
  (TEST_RR_SRC2_EQ_DEST 23 'sll #x00004000 #x00000001 14 );
  (TEST_RR_SRC12_EQ_DEST 24 'sll 24 3 );

  ;-------------------------------------------------------------
  ; Bypassing tests
  ;-------------------------------------------------------------

  (TEST_RR_DEST_BYPASS 25 0 'sll #x0000000000000080 #x0000000000000001 7  );
  (TEST_RR_DEST_BYPASS 26 1 'sll #x0000000000004000 #x0000000000000001 14 );
  (TEST_RR_DEST_BYPASS 27 2 'sll #x0000000080000000 #x0000000000000001 31 );

  (TEST_RR_SRC12_BYPASS 28 0 0 'sll #x0000000000000080 #x0000000000000001 7  );
  (TEST_RR_SRC12_BYPASS 29 0 1 'sll #x0000000000004000 #x0000000000000001 14 );
  (TEST_RR_SRC12_BYPASS 30 0 2 'sll #x0000000080000000 #x0000000000000001 31 );
  (TEST_RR_SRC12_BYPASS 31 1 0 'sll #x0000000000000080 #x0000000000000001 7  );
  (TEST_RR_SRC12_BYPASS 32 1 1 'sll #x0000000000004000 #x0000000000000001 14 );
  (TEST_RR_SRC12_BYPASS 33 2 0 'sll #x0000000080000000 #x0000000000000001 31 );

  (TEST_RR_SRC21_BYPASS 34 0 0 'sll #x0000000000000080 #x0000000000000001 7  );
  (TEST_RR_SRC21_BYPASS 35 0 1 'sll #x0000000000004000 #x0000000000000001 14 );
  (TEST_RR_SRC21_BYPASS 36 0 2 'sll #x0000000080000000 #x0000000000000001 31 );
  (TEST_RR_SRC21_BYPASS 37 1 0 'sll #x0000000000000080 #x0000000000000001 7  );
  (TEST_RR_SRC21_BYPASS 38 1 1 'sll #x0000000000004000 #x0000000000000001 14 );
  (TEST_RR_SRC21_BYPASS 39 2 0 'sll #x0000000080000000 #x0000000000000001 31 );

  (TEST_RR_ZEROSRC1 40 'sll 0 15 );
  (TEST_RR_ZEROSRC2 41 'sll 32 32 );
  (TEST_RR_ZEROSRC12 42 'sll 0 );
  (TEST_RR_ZERODEST 43 'sll 1024 2048 );

  



  


  



)