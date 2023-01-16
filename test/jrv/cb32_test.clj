(ns jrv.cb32-test
  (:refer-clojure :rename {and cj-and, or cj-or, not cj-not, rem cj-rem})
  (:require [clojure.test :refer :all]
   [jrv.korr [cj-utils :refer :all]
             [env :refer :all]
             [reg :refer :all]
             [fmt :refer :all]]
   [jrv.ins.cb32 :refer :all]
   [jrv.core-test :refer :all]
  ))

(deftest c-nop
  (is (jrv-c= (nop) 2r0000000000000001)))


(deftest c-addi ;; rd non-zero sign extended imm6
  (is (jrv-c= (addi x1  1    ) 2r0000000010000101))
  (is (jrv-c= (addi x31 1    ) 2r0000111110000101))
  (is (jrv-c= (addi x1  -1   ) 2r0001000011111101))
  (is (jrv-c= (addi x31 -1   ) 2r0001111111111101))
  (is (jrv-c= (addi x1  0x1F ) 2r0000000011111101))
  (is (jrv-c= (addi x31 0x1F ) 2r0000111111111101))
  (is (jrv-c= (addi x1  -0x20) 2r0001000010000001))
  (is (jrv-c= (addi x1  20   ) 2r0000000011010001)))


(deftest c-li ;; rd sign extended imm6
  (is (jrv-c= (li x1  0    ) 2r0100000010000001))
  (is (jrv-c= (li x31 0    ) 2r0100111110000001))
  (is (jrv-c= (li x1  0x1f ) 2r0100000011111101)) ;;011111
  (is (jrv-c= (li x31 0x1f ) 2r0100111111111101))
  (is (jrv-c= (li x1  1    ) 2r0100000010000101))
  (is (jrv-c= (li x1  10   ) 2r0100000010101001))
  (is (jrv-c= (li x1  -1   ) 2r0101000011111101)) ;;111111
  (is (jrv-c= (li x1  -0x20) 2r0101000010000001)))


(deftest c-addi4spn ;; non-zero zero extended imm10 multiple of 4
  (is (jrv-c= (addi4spn x8  4   ) 2r0000000001000000))
  (is (jrv-c= (addi4spn x15 4   ) 2r0000000001011100))
  (is (jrv-c= (addi4spn x8  1020) 2r0001111111100000))
  (is (jrv-c= (addi4spn x15 1020) 2r0001111111111100))
  (is (jrv-c= (addi4spn x8  0x20) 2r0001000000000000))
  (is (jrv-c= (addi4spn x8  0xfc) 2r0001100111100000))
  (is (jrv-c= (addi4spn x8  0xc ) 2r0000000001100000))
  (is (jrv-c= (addi4spn x8  20  ) 2r0000100001000000)))


(deftest c-addi16sp ;; non-zero sign extended imm10 multiple of 16 (0x10/) 2r10000))
  (is (jrv-c= (addi16sp 0x10  ) 2r0110000101000001))
  (is (jrv-c= (addi16sp -0x10 ) 2r0111000101111101))
  (is (jrv-c= (addi16sp 0x1f0 ) 2r0110000101111101))
  (is (jrv-c= (addi16sp -0x200) 2r0111000100000001))
  (is (jrv-c= (addi16sp 0xf0  ) 2r0110000101101101))
  (is (jrv-c= (addi16sp -0xf0 ) 2r0111000101010001))
  (is (jrv-c= (addi16sp 0x20  ) 2r0110000100000101))
  (is (jrv-c= (addi16sp -0x20 ) 2r0111000100111101)))


(deftest c-lui ;; rd non-zero sign extended imm17 multiple of 4096 (0x1000))
  (is (jrv-c= (lui x1  0x1000  ) 2r0110000010000101))
  (is (jrv-c= (lui x31 0x1000  ) 2r0110111110000101))
  (is (jrv-c= (lui x1  -0x1000 ) 2r0111000011111101))
  (is (jrv-c= (lui x31 -0x1000 ) 2r0111111111111101))
  (is (jrv-c= (lui x1  0xf000  ) 2r0110000010111101))
  (is (jrv-c= (lui x1  0x2000  ) 2r0110000010001001))
  (is (jrv-c= (lui x1  -0xf000 ) 2r0111000011000101)) ;;??
  (is (jrv-c= (lui x1  -0x10000) 2r0111000011000001)))


(deftest c-andi ;; rd sign extended imm6
  (is (jrv-c= (andi x8  0    ) 2r1000100000000001))
  (is (jrv-c= (andi x15 0    ) 2r1000101110000001))
  (is (jrv-c= (andi x8  -1   ) 2r1001100001111101))
  (is (jrv-c= (andi x15 -1   ) 2r1001101111111101))
  (is (jrv-c= (andi x8  -0x20) 2r1001100000000001))
  (is (jrv-c= (andi x15 -0x20) 2r1001101110000001))
  (is (jrv-c= (andi x8  0x1f ) 2r1000100001111101))
  (is (jrv-c= (andi x15 0x1f ) 2r1000101111111101)))


(deftest c-srli
  (is (jrv-c= (srli x8  1 ) 2r1000000000000101))
  (is (jrv-c= (srli x15 1 ) 2r1000001110000101))
  (is (jrv-c= (srli x8  31) 2r1000000001111101))
  (is (jrv-c= (srli x15 31) 2r1000001111111101))
  (is (jrv-c= (srli x8  7 ) 2r1000000000011101))
  (is (jrv-c= (srli x8  15) 2r1000000000111101))
  (is (jrv-c= (srli x8  25) 2r1000000001100101))
  (is (jrv-c= (srli x8  8 ) 2r1000000000100001)))


(deftest c-srai
  (is (jrv-c= (srai x8  1 ) 2r1000010000000101))
  (is (jrv-c= (srai x15 1 ) 2r1000011110000101))
  (is (jrv-c= (srai x8  31) 2r1000010001111101))
  (is (jrv-c= (srai x15 31) 2r1000011111111101))
  (is (jrv-c= (srai x8  7 ) 2r1000010000011101))
  (is (jrv-c= (srai x8  15) 2r1000010000111101))
  (is (jrv-c= (srai x8  25) 2r1000010001100101))
  (is (jrv-c= (srai x8  8 ) 2r1000010000100001)))


(deftest c-slli
  (is (jrv-c= (slli x1  1 ) 2r0000000010000110))
  (is (jrv-c= (slli x31 1 ) 2r0000111110000110))
  (is (jrv-c= (slli x1  31) 2r0000000011111110))
  (is (jrv-c= (slli x31 31) 2r0000111111111110))
  (is (jrv-c= (slli x1  7 ) 2r0000000010011110))
  (is (jrv-c= (slli x1  5 ) 2r0000000010010110))
  (is (jrv-c= (slli x1  25) 2r0000000011100110))
  (is (jrv-c= (slli x1  8 ) 2r0000000010100010)))


(deftest c-mv
  (is (jrv-c= (mv x1  x31) 2r1000000011111110))
  (is (jrv-c= (mv x31 x1 ) 2r1000111110000110))
  (is (jrv-c= (mv x1  x31) 2r1000000011111110))
  (is (jrv-c= (mv x31 x1 ) 2r1000111110000110))
  (is (jrv-c= (mv x1  x1 ) 2r1000000010000110))
  (is (jrv-c= (mv x3  x1 ) 2r1000000110000110))
  (is (jrv-c= (mv x1  x2 ) 2r1000000010001010))
  (is (jrv-c= (mv x4  x8 ) 2r1000001000100010)))


(deftest c-add
  (is (jrv-c= (add x1  x31) 2r1001000011111110))
  (is (jrv-c= (add x31 x1 ) 2r1001111110000110))
  (is (jrv-c= (add x1  x31) 2r1001000011111110))
  (is (jrv-c= (add x31 x1 ) 2r1001111110000110))
  (is (jrv-c= (add x1  x1 ) 2r1001000010000110))
  (is (jrv-c= (add x3  x1 ) 2r1001000110000110))
  (is (jrv-c= (add x1  x2 ) 2r1001000010001010))
  (is (jrv-c= (add x4  x8 ) 2r1001001000100010)))


(deftest c-sub
  (is (jrv-c= (sub x8  x15) 2r1000110000011101))
  (is (jrv-c= (sub x15 x8 ) 2r1000111110000001))
  (is (jrv-c= (sub x8  x8 ) 2r1000110000000001))
  (is (jrv-c= (sub x15 x15) 2r1000111110011101))
  (is (jrv-c= (sub x10 x11) 2r1000110100001101))
  (is (jrv-c= (sub x11 x10) 2r1000110110001001))
  (is (jrv-c= (sub x14 x11) 2r1000111100001101))
  (is (jrv-c= (sub x11 x14) 2r1000110110011001)))


(deftest c-xor
  (is (jrv-c= (xor x8  x15) 2r1000110000111101))
  (is (jrv-c= (xor x15 x8 ) 2r1000111110100001))
  (is (jrv-c= (xor x8  x8 ) 2r1000110000100001))
  (is (jrv-c= (xor x15 x15) 2r1000111110111101))
  (is (jrv-c= (xor x10 x11) 2r1000110100101101))
  (is (jrv-c= (xor x11 x10) 2r1000110110101001))
  (is (jrv-c= (xor x14 x11) 2r1000111100101101))
  (is (jrv-c= (xor x11 x14) 2r1000110110111001)))


(deftest c-or
  (is (jrv-c= (or x8  x15) 2r1000110001011101))
  (is (jrv-c= (or x15 x8 ) 2r1000111111000001))
  (is (jrv-c= (or x8  x8 ) 2r1000110001000001))
  (is (jrv-c= (or x15 x15) 2r1000111111011101))
  (is (jrv-c= (or x10 x11) 2r1000110101001101))
  (is (jrv-c= (or x11 x10) 2r1000110111001001))
  (is (jrv-c= (or x14 x11) 2r1000111101001101))
  (is (jrv-c= (or x11 x14) 2r1000110111011001)))


(deftest c-and
  (is (jrv-c= (and x8  x15) 2r1000110001111101))
  (is (jrv-c= (and x15 x8 ) 2r1000111111100001))
  (is (jrv-c= (and x8  x15) 2r1000110001111101))
  (is (jrv-c= (and x8  x8 ) 2r1000110001100001))
  (is (jrv-c= (and x15 x15) 2r1000111111111101))
  (is (jrv-c= (and x10 x11) 2r1000110101101101))
  (is (jrv-c= (and x11 x10) 2r1000110111101001))
  (is (jrv-c= (and x14 x11) 2r1000111101101101)))


(deftest c-j ;; sign extended imm12 multiple of 2
  (is (jrv-c= (j 0     ) 2r1010000000000001))
  (is (jrv-c= (j 0x7fe ) 2r1010111111111101))   ;;011111111110
  (is (jrv-c= (j -0x800) 2r1011000000000001))   ;;100000000000
  (is (jrv-c= (j 0xfe  ) 2r1010100011111101))   ;;000011111110
  (is (jrv-c= (j 0x7e  ) 2r1010100010111101))   ;;000001111110
  (is (jrv-c= (j 2     ) 2r1010000000001001))   ;;000000000010
  (is (jrv-c= (j -2    ) 2r1011111111111101))   ;;111111111110
  (is (jrv-c= (j 20    ) 2r1010100000010001))) ;;000000010100


(deftest c-jal ;; sign extended imm12 multiple of 2
  ( is (jrv-c= (jal 0     ) 2r0010000000000001))
  ( is (jrv-c= (jal 0x7fe ) 2r0010111111111101))
  ( is (jrv-c= (jal -0x800) 2r0011000000000001))
  ( is (jrv-c= (jal 0xfe  ) 2r0010100011111101))
  ( is (jrv-c= (jal 0x7e  ) 2r0010100010111101))
  ( is (jrv-c= (jal 2     ) 2r0010000000001001))
  ( is (jrv-c= (jal -2    ) 2r0011111111111101))
  ( is (jrv-c= (jal 20    ) 2r0010100000010001)))


(deftest c-jr ;; check orientation
  (is (jrv-c= (jr x1 ) 2r1000000010000010))
  (is (jrv-c= (jr x31) 2r1000111110000010))
  (is (jrv-c= (jr x2 ) 2r1000000100000010))
  (is (jrv-c= (jr x30) 2r1000111100000010))
  (is (jrv-c= (jr x4 ) 2r1000001000000010))
  (is (jrv-c= (jr x5 ) 2r1000001010000010))
  (is (jrv-c= (jr x7 ) 2r1000001110000010))
  (is (jrv-c= (jr x8 ) 2r1000010000000010)))


(deftest c-jalr  ;; check orientation
  (is (jrv-c= (jalr x1 ) 2r1001000010000010))
  (is (jrv-c= (jalr x31) 2r1001111110000010))
  (is (jrv-c= (jalr x2 ) 2r1001000100000010))
  (is (jrv-c= (jalr x30) 2r1001111100000010))
  (is (jrv-c= (jalr x4 ) 2r1001001000000010))
  (is (jrv-c= (jalr x5 ) 2r1001001010000010))
  (is (jrv-c= (jalr x7 ) 2r1001001110000010))
  (is (jrv-c= (jalr x8 ) 2r1001010000000010)))


(deftest c-beqz ;; rd  sign extended imm9 multiple of 2
  (is (jrv-c= (beqz x8  0     ) 2r1100000000000001))
  (is (jrv-c= (beqz x15 0     ) 2r1100001110000001))
  (is (jrv-c= (beqz x8  2     ) 2r1100000000001001))
  (is (jrv-c= (beqz x15 2     ) 2r1100001110001001))
  (is (jrv-c= (beqz x8  -2    ) 2r1101110001111101))   ;;111111110
  (is (jrv-c= (beqz x15 -2    ) 2r1101111111111101))
  (is (jrv-c= (beqz x8  0xfe  ) 2r1100110001111101))   ;;011111110
  (is (jrv-c= (beqz x8  -0x100) 2r1101000000000001))) ;;100000000


(deftest c-bnez ;; rd  sign extended imm9 multiple of 2
  (is (jrv-c= (bnez x8  0     ) 2r1110000000000001))
  (is (jrv-c= (bnez x15 0     ) 2r1110001110000001))
  (is (jrv-c= (bnez x8  2     ) 2r1110000000001001))
  (is (jrv-c= (bnez x15 2     ) 2r1110001110001001))
  (is (jrv-c= (bnez x8  -2    ) 2r1111110001111101))
  (is (jrv-c= (bnez x15 -2    ) 2r1111111111111101))
  (is (jrv-c= (bnez x8  0xfe  ) 2r1110110001111101))
  (is (jrv-c= (bnez x8  -0x100) 2r1111000000000001)))


(deftest c-lv ;; rd rb (zero extended imm7 mutible of 4))
  (is (jrv-c= (lv x8  x15 0   ) 2r0100001110000000))
  (is (jrv-c= (lv x15 x8  0   ) 2r0100000000011100))
  (is (jrv-c= (lv x8  x15 0x7c) 2r0101111111100000))
  (is (jrv-c= (lv x15 x8  0x7c) 2r0101110001111100))
  (is (jrv-c= (lv x8  x8  0x3c) 2r0101110001000000))
  (is (jrv-c= (lv x8  x8  0x1c) 2r0100110001000000))
  (is (jrv-c= (lv x8  x8  0xc ) 2r0100010001000000))
  (is (jrv-c= (lv x8  x8  0x58) 2r0100110000100000)))


(deftest c-sv ;; rs rb (zero extended imm7 mutible of 4))
  (is (jrv-c= (sv x8  x15 0   ) 2r1100001110000000))
  (is (jrv-c= (sv x15 x8  0   ) 2r1100000000011100))
  (is (jrv-c= (sv x8  x15 0x7c) 2r1101111111100000)) ;;1111100
  (is (jrv-c= (sv x15 x8  0x7c) 2r1101110001111100))
  (is (jrv-c= (sv x8  x8  0x3c) 2r1101110001000000)) ;;0111100
  (is (jrv-c= (sv x8  x8  0x1c) 2r1100110001000000)) ;;0011100
  (is (jrv-c= (sv x8  x8  0xc ) 2r1100010001000000)) ;;0001100
  (is (jrv-c= (sv x8  x8  0x58) 2r1100110000100000))) ;; 1011000


(deftest c-lvsp ;; rd (zero extended imm8 multiple of 4
  (is (jrv-c= (lvsp x1  0   ) 2r0100000010000010))
  (is (jrv-c= (lvsp x31 0   ) 2r0100111110000010))
  (is (jrv-c= (lvsp x1  0xfc) 2r0101000011111110))
  (is (jrv-c= (lvsp x31 0xfc) 2r0101111111111110))
  (is (jrv-c= (lvsp x1  0x7c) 2r0101000011110110))
  (is (jrv-c= (lvsp x1  0x1c) 2r0100000011110010))
  (is (jrv-c= (lvsp x1  0xc ) 2r0100000010110010))
  (is (jrv-c= (lvsp x1  0x58) 2r0100000011100110)))


(deftest c-svsp ;; rb (zero extended imm8 multiple of 4))
  (is (jrv-c= (svsp x0  0   ) 2r1100000000000010))
  (is (jrv-c= (svsp x31 0   ) 2r1100000001111110))
  (is (jrv-c= (svsp x0  0xfc) 2r1101111110000010)) ;; 11111100
  (is (jrv-c= (svsp x31 0xfc) 2r1101111111111110))
  (is (jrv-c= (svsp x0  0x7c) 2r1101111010000010)) ;; 01111100
  (is (jrv-c= (svsp x0  0x1c) 2r1100111000000010)) ;; 00011100
  (is (jrv-c= (svsp x0  0xc ) 2r1100011000000010)) ;; 00001100
  (is (jrv-c= (svsp x0  0x58) 2r1100110010000010))) ;; 01011000


(deftest c-ebreak
  (is (jrv-c= (ebreak) 2r1001000000000010)))
