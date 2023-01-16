(ns jrv.ins.i32
  "32-bit base Instructions for the jrv RISC-V assembler."
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


        ;;;; Integer computational instructions ;;;;

(def-vi addi
  "Add Immediate: Sign-extended 12-bit immediate `imm12` to register length
   and add to `rs` placing the result in `rd`."
  [rd rs imm12]
  (immed imm12 rs 0 rd 0x13))

(def-vi lui
 "Load Upper Immediate: Load most significant bits of register `rd` with
  immediate `imm32`which should be a multiple of 4096 or 0x1000,
  filling the lower 12 bits with zeros"
  [rd imm32]
  (upperimm imm32 rd 0x37))

(def-vi auipc
  "Add Upper Immediate to PC: Add the immediate `imm`
   (which should be a multiple of 4096 or 0x1000) to the program counter
   and load the result into the register `rd`."
  [rd imm]
  (let [ofst (offset imm)]
    (upperimm ofst rd 0x17)))

(def-vi slti
 "Set if less than immediate: If `rs` is less than the 12-bit immediate
  (sign-extended to register length), Load `rd` with value 1."
  [rd rs imm12]
  (immed imm12 rs 2 rd 0x13))


(def-vi sltiu
 "Set if less than immediate unsigend: If `rs1` is less than the unsigned
  representation of 12-bit sign-extended immediate, load `rd` with value 1."
 [rd rs1 imm12]
  (immed imm12 rs1 3 rd 0x13))  ;; turns out 'immed' is ok
;; ;;) ;TODO 0 - 8191 WATCH!! changed to uimmp = 4095!! changed to immp !!!!


(def-vi xori
 "XOR 12-bit immediate (sign-extended to register length) with `rs1` and
  load `rd` with result."
  [rd rs1 imm12]
  (immed imm12 rs1 4 rd 0x13))


(def-vi ori
 "OR 12-bit immediate (sign-extended to register length) with `rs1` and
  load `rd` with result."
  [rd rs1 imm12]
  (immed imm12 rs1 6 rd 0x13))


(def-vi andi
 "AND 12-bit immediate (sign-extended to register length) with `rs1` and load `rd`
  with result."
  [rd rs1 imm12]
  (immed imm12 rs1 7 rd 0x13))


(def-vi slli
 "Shift left logical immediate: Shift the contents of `rs1` left by the immediate
  value (between 0 and 31) and load `rd` with result."
  [rd rs1 imm5]
  (if (<= 0 imm5 31)
      (build-rv-ins [7 5 5 3 5 7] 0 (bit-field imm5 4 0) (regno rs1) 1
                                                  (regno rd) 0x13)
      (cpc-ex "Error slli immediate not between 0 and 31."))) ;c

(def-vi srli
 "Shift right logical immediate: Shift the contents of `rs1` right by the
  immediate value (between 0 and 31) and load `rd` with result."
  [rd rs1 imm5]
  (if (<= 0 imm5 31)
      (build-rv-ins [7 5 5 3 5 7] 0 (bit-field imm5 4 0) (regno rs1) 5
                                                  (regno rd) 0x13)
      (cpc-ex "Error srli immediate not between 0 and 31.")));c

(def-vi srai
 "Shift right arithmetic immediate: Shift the contents of `rs1` right by the
  immediate value (between 0 and 31) keeping the sign bit and load `rd` with result."
  [rd rs1 imm5]
  (if (<= 0 imm5 31)
      (build-rv-ins [7 5 5 3 5 7] 0x20 (bit-field imm5 4 0)
                                                     (regno rs1) 5
                                                     (regno rd) 0x13)
      (cpc-ex "Error srai immediate not between 0 and 31."))) ;c

        ;;;; Register only Computational Instructions;;;;

(def-vi add
 "Load `rd` with the sum of the contents of `rs1` and `rs2`."
  [rd rs1 rs2]
  (register 0 rs2 rs1 0 rd 0x33))
   ;cc

(def-vi sub
 "Load `rd` with the result of subracting the contents of `rs2` from `rs1`."
  [rd rs1 rs2]
 (register 0x20 rs2 rs1 0 rd 0x33))
   ;c

(def-vi sll
 "Shift left logical: Load `rd` with the result of shifting the contents of `rs1`
  left by the amount in `rs2` which must contain a number between 0 and 31."
  [rd rs1 rs2]
 (register 0 rs2 rs1 1 rd 0x33))


(def-vi slt
 "Set if less than: Set the value of `rd` to 1 if the value in `rs1` is less that
  the value in `rs2`, using signed comparison."
  [rd rs1 rs2]
 (register 0 rs2 rs1 2 rd 0x33))


(def-vi sltu
 "Set if less than unsigned: Set the value of `rd` to 1 if the value in `rs1` is
  less that the value in `rs2`, using unsigned comparison."
  [rd rs1 rs2]
 (register 0 rs2 rs1 3 rd 0x33))


(def-vi xor
 "Load `rd` with the result of the logical xor of `rs1` and `rs2`."
  [rd rs1 rs2]
  (register 0 rs2 rs1 4 rd 0x33)) ;c

(def-vi srl
 "Shift right logical: Load `rd` with the result of shifting the contents of `rs1`
  right by the amount in `rs2` which must contain a number between 0 and 31."
  [rd rs1 rs2]
 (register 0 rs2 rs1 5 rd 0x33))


(def-vi sra
  "Shift right arithmetic: Load `rd` with the result of shifting the contents of
   `rs1` right by the amount in `rs2` which must contain a number between 0 and 31.
   Keeping the sign bit."
  [rd rs1 rs2]
  (register 0x20 rs2 rs1 5 rd 0x33) ; 5 was orignally 2)
            )


(def-vi or
 "Load `rd` with the result of the logical or of `rs1` and `rs2`."
 [rd rs1 rs2]
 (register 0 rs2 rs1 6 rd 0x33)) ;c

(def-vi and
 "Load `rd` with the result of the logical and of `rs1` and `rs2`."
 [rd rs1 rs2]
 (register 0 rs2 rs1 7 rd 0x33)) ;c

        ;;;; Jumps and branches ;;;;

(def-vi jal
  "Jump and link: The return address (the address following the JAL e pc+4)
   is stored in `rd`. The destination address is the sum of
   The sign extended immediate `imm` (a multiple of 2) and the current address
   of the jump instruction."
  [rd imm]
  (jump imm rd)) ;ccc

(def-vi jalr
  "Jump and link register: The return address
   (the address following the JALR e. pc+4) is stored in `rd`.
   The destination address is the sum of the sign extended immediate and
   the address stored in `rs1`."
  [rd rs1 imm12]
  (immed imm12 rs1 0 rd 0x67)) ;c

(def-vi beq
  "Branch if equal: If `rs1` is equal to `rs2`, the 12-bit immediate is sign-extended
   added to the value of the Program Counter
   (The value of the address following the instruction)
   to form the destination address."
  [rs1 rs2 imm12]
  (branch imm12 rs2 rs1 0 0x63)) ;c

(def-vi bne
  "Branch if not equal: If `rs1` is not equal to `rs2`, the immediate is sign-extended
   and added to the value of the Program Counter
   (The value of the address following the instruction)
   to form the destination address."
  [rs1 rs2 imm12]
  (branch imm12 rs2 rs1 1 0x63)) ;c

(def-vi blt
  "Branch if less than: If `rs1` is less than `rs2` (using signed comparison),
   the immediate is sign-extended and added to the value of the Program Counter
   (The value of the address following the instruction)
   to form the destination address."
  [rs1 rs2 imm12]
  (branch imm12 rs2 rs1 4 0x63))


(def-vi bge
  "Branch if greater or equal: If `rs1` is greater than or equal to `rs2` (using
   signed comparison),the immediate is sign-extended and added to the value of
   the Program Counter (The value of the address following the instruction)
   to form the destination address."
  [rs1 rs2 imm12]
  (branch imm12 rs2 rs1 5 0x63))


(def-vi bltu
  "Branch if less than unsigned: If `rs1` is less than `rs2` (using unsigned
   comparison),the immediate is sign-extended and added to the value of
   the Program Counter (The value of the address following the instruction)
   to form the destination address."
  [rs1 rs2 imm12]
  (branch imm12 rs2 rs1 6 0x63))


(def-vi bgeu
  "Branch if greater or equal: If `rs1` is greater than or equal to `rs2` (using
   unsigned comparison), the immediate is sign-extended and added to the value of
   the Program Counter (The value of the address following the instruction)."
  [rs1 rs2 imm12]
  (branch imm12 rs2 rs1 7 0x63))


        ;;;; Loads and stores ;;;;


(def-vi lb
  "Load byte: The 12-bit immediate is added to the value of `rs1` to form a
   memory address An 8-bit value (byte) is fetched from this address and loaded
   into `rd` The value is sign exteded to the full length of the register.
   Note: Syntax different from standard."
  [rd rs1 imm12]
  (immed imm12 rs1 0 rd 3))


(def-vi lk
  "Load jait: The 12-bit immediate is added to the value of `rs1` to form a memory
   address. A 16-bit value (jait) is fetched from this address and loaded into `rd`.
   The value is sign exteded to the full length of the register.
   Note: Syntax different from standard."
  [rd rs1 imm12]
  (immed imm12 rs1 1 rd 3))


(def-vi lv
  "Load vait: The 12-bit immediate is added to the value of `rs1` to form a memory
   address. A 32-bit value (vait) is fetched from this address and loaded into `rd`.
   The value is sign exteded to the full length of the register.
   Note: Syntax different from standard."
  [rd rs1 imm12]
  (immed imm12 rs1 2 rd 3)) ;cc

(def-vi lbu
  "Load unsigned byte: The 12-bit immediate is added to the value of `rs1` to
   form a memory address An 8-bit value is fetched from this address and loaded
   into `rd`. The value is zero exteded to the full length of the register
   Note: Syntax different from standard."
  [rd rs1 imm12]
  (immed imm12 rs1 4 rd 3))


(def-vi lku
  "Load unsigned jait: The 12-bit immediate is added to the value of `rs1` to
   form a memory address A 16-bit value is fetched from this address and loaded
   into `rd`. The value is zero exteded to the full length of the register.
   Note: Syntax different from standard."
  [rd rs1 imm12]
  (immed imm12 rs1 5 rd 3))


(def-vi sb
  "Store byte: The 12-bit immediate is added to the value of rb to form a memory
   address The least significant 8-bit value is copied from rs and stored at
   this address. Note: Syntax different from standard."
  [rs rb imm12]
  (store imm12 rs rb 0))


(def-vi sk
  "Store jait: The 12-bit immediate is added to the value of `rb` to form a memory
   address The least significant 16-bit value is copied from `rs` and stored at
   this address. Note: Syntax different from standard."
  [rs rb imm12]
  (store imm12 rs rb 1))


(def-vi sv
  "Store vait: The 12-bit immediate is added to the value of `rs2` to form a memory
   address The least significant 32-bit value is copied from `rs1` and stored at
   this address. Note: Syntax different from standard."
  [rs rb imm12]
  (store imm12 rs rb 2)) ;cc

        ;;;; Miscellaneous Instructions ;;;;


(def-vi fence
  "(fence succ pred)"
  [succ pred]
  (if (cj-and (signed-int? pred 4) (signed-int? succ 4))
      (build-rv-ins [4 4 4 5 3 5 7] 0 pred succ 0 0 0 0xF))
      (cpc-ex "Fence immediate values out of range."))

;; (def-vi fence.i [)]

(def-vi ecall
  "Environment Call"
  []
  (build-rv-ins [16 16] 0 0x73))

(def-vi ebreak
  "Environment Break"
  []
  (build-rv-ins [16 16] 0x10 0x73)) ;c
