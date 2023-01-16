(ns jrv.ins.mul32
  "32-bit Multiplication and Division Instructions for the jrv RISC-V assembler."
   {:author "Kyuvi"
    :license {:name "GPL-3.0 WITH Classpath-exception-2.0"
              :url "https://www.gnu.org/licenses/gpl-3.0.html"}}
  ;; not strictly neccesary, but might as well set a project wide standard
  (:refer-clojure :rename {and cj-and, or cj-or, not cj-not, rem cj-rem})
  (:require [jrv.korr
             [cj-utils :refer :all]
             [env :refer :all]
             [reg :refer :all]
             [fmt :refer :all]
             ])
  )

(def-vi mul
  "Multiply the contents of `rs1` and `rs2` and load rd with the result.
  Overflow is ignored, so rd is only loaded with the lower half of the result.
  rd ≠ rs1 or rs2."
  [rd rs1 rs2]
  (muldiv rs2 rs1 0 rd 0x33)
)

(def-vi mulh
  "Multiply the contents of `rs1` and `rs2` and load rd with the
  upper half (high bits) of the result of the signed multiplication.
  rd ≠ rs1 or rs2."
  [rd rs1 rs2]
  (muldiv rs2 rs1 1 rd 0x33)
)

(def-vi mulhsu
  "Multiply the contents of `rs1` and `rs2` and load rd with the
  upper half (high bits) of the result of the multiplication of
  a signed rs1 and an unsigned rs2. rd ≠ rs1 or rs2."
  [rd rs1 rs2]
  (muldiv rs2 rs1 2 rd 0x33)
)

(def-vi mulhu
  "Multiply the contents of `rs1` and `rs2` and load rd with the
  upper half (high bits) of the result of the multiplication of
  a signed rs1 and an unsigned rs2. rd ≠ rs1 or rs2."
  [rd rs1 rs2]
  (muldiv rs2 rs1 3 rd 0x33)
)

(def-vi div
  "Divide the contents of `rs1` by the contents of `rs2` and
  load rd with the quotient. Both values are signed.
  Errors: -2^(L-1)/-1=-2^(L-1)(L = register length, should be +2^(L-1).),
  x/0=-1(#hffffffff)."
  [rd rs1 rs2]
  (muldiv rs2 rs1 4 rd 0x33)
)

(def-vi divu
  "Divide the contents of `rs1` by the contents of `rs2` and
  load rd with the quotient. Both values are unsigned.
  Errors: x/0=+2^(L-1)-1 (L = register length)."
  [rd rs1 rs2]
  (muldiv rs2 rs1 5 rd 0x33)
)

(def-vi rem
  "Divide the contents of `rs1` by the contents of `rs2` and
  load rd with the remainder. Both values are signed.
  Errors: -2^(L-1)/-1=0(L = register length, correct result), x/0=x."
  [rd rs1 rs2]
  (muldiv rs2 rs1 6 rd 0x33)
)

(def-vi remu
  "Divide the contents of `rs1` by the contents of `rs2` and
  load rd with the quotient. Both values are unsigned.
  Errors:x/0=x."
  [rd rs1 rs2]
  (muldiv rs2 rs1 7 rd 0x33)
)
