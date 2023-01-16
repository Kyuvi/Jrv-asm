(ns jrv.ins.cb32
  "32-bit compressed base Instructions for the jrv RISC-V assembler."
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

;addi x0 x0 0(?)
(def-ci nop
 "Takes one cycle and advances program-counter by 2"
  []
  (build-rv-ins [16] 1 ))


        ;;;; Integer Computational Instructions ;;;;

;; addi - add immediate (addi rd rd value)
(def-ci addi
  "Add 6-bit immediate (sign extended to register width) to the value in rd
  then write the result to rd.
  (addi rd rd imm6) rd ≠ 0, imm6 ≠ 0."
  [rd imm6]
  (if (cj-not (zero? (regno rd)))
       (hold-expr :c.addi [imm6]
              (if (cj-not (zero? imm6))
                  (cimm imm6 rd 0 1)
                  (cpc-ex "c.addi: Immediate can not be 0.")))
      (cpc-ex "c.addi: Can not use x0 as the destination register.")))


;; c.addi4spn - add immediate to stack pointer (addi rd x2 value)
;; value ≠ 0 // rd = x8..x15
(def-ci addi4spn
  "addi4spn: Load rd with the sum of the stack pointer(x2) and the 10-bit
  immediate, which should be a multiple of 4 that is zero extended to register
  width. (addi rd x2 imm) rd = x8..x15, imm ≠ 0."
  [crd imm10]
  (let  [addr (pc)]
    (if (c-reg? crd)
         (hold-expr :c.addi4spn [imm10]
           (cond (zero? imm10) ;(= imm10 0)
                  (asm-ex addr "c.addi4spn: Immediate can not be 0.")
                  (cj-not (unsigned-int? imm10 10)) ;; Note: unsigned-int?!!
                  (asm-ex addr "c.addi4spn: Immediate out of range.")
                  (cj-not (zero? (bit-and imm10 0x3)))
                  (asm-ex "c.addi4spn: Immediate should be a multiple of 4."
                          addr)
                 :else
                 (build-rv-ins [3 2 4 1 1 3 2]
                                0 (bit-field imm10 5 4) (bit-field imm10 9 6)
                                (bit-field imm10 2) (bit-field imm10 3)
                                (cregno crd) 0)
                  ))
        (asm-ex addr "c.addi4spn: Invalid compressed register (use x8..x15).")))
    )
  ;; (if ;(cj-and
  ;;      (c-reg? rd) ;(cj-not (= 0 imm8))) ; TODO move to ciwid
  ;;     (ciwid rd imm8 0 0)
  ;;     (cpc-ex "Invalid c.addi4spn expression.")
  ;; ))


;; addi16sp - add immediate to stack pointer (addi x2 x2 value)
;; // imm ≠ 0
(def-ci addi16sp
  "addi16sp: Load stack pointer(x2) with the sum of the current value of the
  stack pointer(x2) and the 10-bit immediate which should be a muliple of 16
  that is sign extended to the register width.
  (addi x2 x2 imm) imm ≠ 0."
  [imm10]
  (let [addr (pc)]
     (hold-expr :c.addi16sp [imm10]
       (cond (zero? imm10);(= imm10 0)
              (asm-ex addr "c.addi16sp: Immediate can not be 0.")
             (cj-not (zero? (bit-and imm10 0xf)))
             (cpc-ex "c.addi16sp: Immediate should be a multiple of 16.")
             (cj-not (signed-int? imm10 10))
             (cpc-ex "c.addi16sp: Immediate value out of range.")
             :else
             (build-rv-ins [3 1 5 1 1 2 1 2]
                               3 (bit-field imm10 9) 2 (bit-field imm10 4)
                                 (bit-field imm10 6) (bit-field imm10 8 7)
                                 (bit-field imm10 5) 1)))
  ))


;; c.li - load immediate (addi rd x0 value)
;; rd ≠ x0
(def-ci li
 "Commpressed Load Immediate: Load 6-bit immediate (sign extended to register
  width) into rd. (addi rd x0 imm) rd ≠ 0."
  [rd imm6]
   (if (cj-not (zero? (regno rd)))
       (cimm imm6 rd 2 1)
       (cpc-ex "c.li: Can not use x0 as the destination register.")))


;; c.lui - load upper immediate (lui rd value)
;; // rd ≠ x0 x2 // imm ≠ 0
(def-ci lui
  "Commpressed load upper immediate: Load rd with non-zero immediate (which should be a
  multiple of 4096 or 0x1000), sign extending the highest bit (bit 18) into
  all the higher bits of rd.
  (lui rd imm18) rd ≠ x0 or x4, imm ≠ 0."
  [rd imm18]
  (let  [addr (pc)]
    (if (cj-not (cj-or (zero? (regno rd)) (= (regno rd) 2)))
         (hold-expr :c.lui [imm18]
                (cond
                  ;; ((or (zero? (regno rd)) (= (regno rd) 2))
                   ;; (cpc-ex "lui: Invalid register (x0 or x2 invalid)"))
                  (cj-not (zero? (bit-and imm18 0xfff)))
                  (asm-ex
                   "c.lui: Immediate should be a multiple of 4096/0x1000." addr)
                  (cj-not (signed-int? imm18 18))
                  (asm-ex addr "c.lui: Immediate value out of range.")
                  (zero? imm18)
                  (asm-ex addr "c.lui: Immediate can not be 0.")
                  :else
                  (build-rv-ins [3 1 5 5 2] 3 (bit-field imm18 17) (regno rd)
                                              (bit-field imm18 16 12) 1)))

        (asm-ex addr "c.lui: Invalid register (x0 and x2 invalid)")
        )))

;; c.andi - logical AND (immediate) (andi rd rd value)
;; //   rd  = x8 ... x15     ?? imm6 ≠ 0 ??
(def-ci andi
 "AND value in rd and the immediate (sign extended to register size) and write
  result to rd. (andi rd rd imm6) rd = x8..x15."
  [crd imm6]
  (let  [addr (pc)]
   (if (c-reg? crd)
       ;; (cismal imm6 rd 4 2 1)
        (hold-expr :c.andi [imm6]
          (cond (cj-not (signed-int? imm6 6))
                 (asm-ex addr "c.andi: Immediate value out of range.")
                :else
                (build-rv-ins [3 1 2 3 5 2] 4 (bit-field imm6 5) 2
                               (cregno crd) (bit-field imm6 4 0) 1))

                 )
     (asm-ex addr "c.andi: Invalid compressed register, (use x8..x15).")))
  )

        ;;;; register shift instructions ;;;;

;; c.srli - shift right logical (immediate) (srli rd rd value)
;; // rd = x8 ... x15 value = 1 .. 31  check summary
(def-ci srli
  "Compressed Shift right logical immediate: Shift contents of register right
  by immediate value and store result in rd.
  (srli rd rd imm), rd = x8..x15, imm = 1 .. 31."
  [crd imm5]
  (let  [addr (pc)]
    (cond (cj-not (c-reg? crd))
          (asm-ex addr "c.srli: Invalid compressed register, (use x8..x15).")
          (cj-not (<= 1 imm5 31))
          (asm-ex addr "c.srli: Immediate should be between 1 and 31.")
          :else
          (build-rv-ins [4 2 3 5 2] 8 0 (cregno crd) imm5 1))
           ))

;; c.srai - shift right arithmetic (Immediate) (srai rd rd value)
;; // rd = x8 ... x15 value = 1 .. 31  check summarY
(def-ci srai
  "Compressed Shift right aritmetic immediate: Shift contents of register right
  by immediate value, keeping the sign bit and store result in rd.
  (srai rd rd imm), rd = x8..x15, imm = 1 .. 31."
  [crd imm5]
  (let  [addr (pc)]
    (cond (cj-not (c-reg? crd))
          (asm-ex addr "c.srai: Invalid compressed register, (use x8..x15).")
          (cj-not (<= 1 imm5 31))
          (asm-ex addr "c.srai: Immediate should be between 1 and 31.")
          :else
          (build-rv-ins [4 2 3 5 2] 8 1 (cregno crd) imm5 1))
           ))


;; c.slli - Shift left logical (immediate) (slli rd rd value)
;; // rd ≠ x0 value = 1 .. 31
(def-ci slli
  "Compressed Shift left logical immediate: Shift contents of register left
  by immediate value and store result in rd.
  (slli rd rd imm), imm = 1 .. 31."
  [rd imm5]
  (let [addr (pc)]
    (cond (zero? (regno rd))
          (asm-ex addr "c.slli: Can not use x0 as the destination register.")
          (cj-not (<= 1 imm5 31))
          (asm-ex addr "c.slli: Immediate should be between 1 and 31.")
          :else
          (build-rv-ins [4 5 5 2] 0 (regno rd) imm5 2)))
  )


        ;;;; Register only Computational instructions ;;;;

;; c.mv - move register to register (add rd x0 r2)
;; // rd  r2 ≠ x0
(def-ci mv
 "Move contents of rs2 to rd,
  rd/rs2 ≠ x0."
  [rd rs2]
  (if (cj-and (cj-not (zero? (regno rd))) (cj-not (zero? (regno rs2))))
      (creg rd rs2 8 2)
      (cpc-ex "c.mv: Register x0 can not be used.")))


;; c.add - add register to register (add rd rd r2)
;; // rd  r2 ≠ x0
(def-ci add
 "Add contents of rs2 to rd and store result in rd.
  (add rd rd rs2), rd/rs2 ≠ 0."
  [rd rs2]
  (if (cj-and (cj-not (zero? (regno rd))) (cj-not (zero? (regno rs2))))
      (creg rd rs2 9 2)
      (cpc-ex "c.add: Register x0 can not be used.")))


;; c.and - AND register to register (and rd rd rs2),
;; //     rd r2 = x8..x15
(def-ci and
 "AND rd and `crs2` and store result in rd.
  (and rd rd rs2) rd/`crs2` = x8..x15."
  [crd crs2]
  (if (cj-and (c-reg? crd) (c-reg? crs2))
      (carith 4 0 3 crd 3 crs2)
      (cpc-ex "c.and: Invalid compressed register, (use x8..x15).")))


;; c.sub - subtract register 2 from destination register (sub rd rd rs2),
;; //     rd r2 = x8 ... x15
(def-ci sub
 "Subtract `crs2` from rd and store result in rd (sub rd rd rs2).
  rd/`crs2`  = x8..x15."
  [crd crs2]
  (if (cj-and (c-reg? crd) (c-reg? crs2))
      (carith 4 0 3 crd 0 crs2)
      (cpc-ex "c.sub: Invalid compressed register, (use x8..x15).")))

;; c.xor - xor register with register (xor rd rd rs2),
;; //     rd r2 = x8 ... x15
(def-ci xor
 "XOR rd and `crs2` and store result in rd.
  (xor rd rd rs2) rd/`crs2`  = x8..x15."
  [crd crs2]
  (if (cj-and (c-reg? crd) (c-reg? crs2))
      (carith 4 0 3 crd 1 crs2)
      (cpc-ex "c.xor: Invalid compressed register, (use x8..x15).")))

;; c.or - OR register to register (or rd rd rs2),
;; //     rd r2 = x8 ... x15
(def-ci or
 "OR rd and `crs2` and store result in rd.
  (or rd rd rs2) rd/`crs2`  = x8..x15."
  [crd crs2]
  (if (cj-and (c-reg? crd) (c-reg? crs2))
      (carith 4 0 3 crd 2 crs2)
      (cpc-ex "c.or: Invalid compressed register, (use x8..x15).")))


        ;;;; Jumps and branches ;;;;

;; c.j - Jump (pc-relative)  (jal  x0 offset)
(def-ci j
  "Compressed jump: Jump to address described by  adding the pc to the
  sign extended immediate.
  Offset should be a multiple of 2 in the range -1024(0x-400) to 1022(0x3FE).
  (jal x0 offset), range = ±2KiB."
  [imm]
  ;; TODO offset ?
;;   (let  [addr (pc]
        ;; (ofst (offset imm)))
    (cjump imm 5 1))

;;  jal x1 offset
(def-ci jal
  "Compressed Jump and link: Jump to address described by adding the pc to the
  sign extended immediate storing return address(pc+2) in x1.
  Offset should be a multiple of 2 in the range -1024(0x-400) to 1022(0x3FE).
  (jal x1 offset), range = ±2KiB."
  [imm]
;;   (let  [addr (pc]
;;         (ofst (offset imm)))
    (cjump imm 1 1))

;; c.jr - jump register (jal x0  r1 0)
;; // r1 ≠ x0
(def-ci jr
 "Compressed jump register: Jump to address contained in register.
  (jalr x0 rs1 0), rs1 ≠ 0."
  [rs1]
  (if (cj-not (zero? (regno rs1)))
      (build-rv-ins [3 1 5 5 2] 4 0 (regno rs1) 0 2)
      (cpc-ex "c.jr: Can not use x0 as source register.")))

;; c.jalr - jump register and link/call (jalr x1  r1 0) ??
;; // r1 ≠ x0
;jalr x1 rs1 0
(def-ci jalr
  "Compressed jump and link register(call): Jump to address contained in register
  and write address following the jump (pc+2) to x1 (link register).
  (jalr x1 rs1 0), rs1 ≠ 0."
  [rs]
  (if (cj-not (zero? (regno rs)))
      (build-rv-ins '(3 1 5 5 2) 4 1 (regno rs) 0 2)
      (cpc-ex "c.jalr: can cj-not use x0 as source register.")))

;; c.beqz - branch if (reg ) equal to zero (beq r1 x0 offset)
(def-ci beqz
 "Branch to target address described by immediate if `crs1` is equal to zero.
  (beq rs1 x0 imm), range = ±256B."
  [crs1 imm]
  (if (c-reg? crs1)
      (cbranch imm crs1 6 1)
      (cpc-ex "c.beqz: Invalid compressed register, (use x8..x15).")))

;; c.bnez  - branch if (reg ) not equal to zero (bne r1 x0 offset)
(def-ci bnez
 "Branch to target address described by immediate if `crs1` is not equal to zero.
  (bne rs1 x0 imm), range = ±256B."
  [crs1 imm]
  (if (c-reg? crs1)
      (cbranch imm crs1 7 1)
      (cpc-ex "c.bnez: Invalid compressed register, (use x8..x15).")))


        ;;;; Loads and stores ;;;;

;; c.lv - load vyte (lv rd r1 offset) //    rd & r1 = x8 ... x15
(def-ci lv
  "Compressed load vait: Load `crd` with the 32bit value from memory location derived
  from adding `crs1` to the 7-bit immediate a multiple of 4 zero extended to the
  register width. (lv rd rs1 imm7) rd/r1 = x8..x15."
  [crd crs1 imm7]
  (let [addr (pc)]
    (if (cj-and (c-reg? crd) (c-reg? crs1))
   ;; (cimm imm8 rd 4 4)
         (hold-expr :c.lv [imm7]
           (cond (cj-not (unsigned-int? imm7 7)) ;; Note: unsigned-int?!!
                  (asm-ex addr "c.lv: Immediate out of range.")
                  (cj-not (zero? (bit-and imm7 0x3)))
                  (asm-ex addr "c.lv: Immediate should be a multiple of 4.")
                 :else
                 (build-rv-ins [3 3 3 1 1 3 2] 2 (bit-field imm7 5 3)
                                (cregno crs1) (bit-field imm7 2)
                                (bit-field imm7 6) (cregno crd) 0)
                  ))
        (asm-ex addr "c.lv: Invalid compressed register (use x8..x15).")))
    )


;; c.sv - store vyte (sw rs2 rs1 offset)
;;    r1 & r2 = x8 ... x15
(def-ci sv
  "Compressed store vait: Stores 32 bit value in rs to memory location derived
  from adding rb to 7-bit immediate, a multiple of 4 zero extended to regsiter
  width. (sv rs rb imm7) rd/r1 = x8..x15."
  [crs crb imm7]
   (let [addr (pc)]
    (if (cj-and (c-reg? crs) (c-reg? crb))
         (hold-expr :c.sv [imm7]
           (cond (cj-not (unsigned-int? imm7 7)) ;; Note: unsigned-int?!!
                 (asm-ex addr "c.sv: Immediate out of range.")
                 (cj-not (zero? (bit-and imm7 0x3)))
                 (asm-ex addr "c.sv: Immediate should be a multiple of 4.")
                 :else
                 (build-rv-ins [3 3 3 1 1 3 2] 6 (bit-field imm7 5 3)
                                (cregno crb) (bit-field imm7 2)
                                (bit-field imm7 6) (cregno crs) 0)
                  ))
        (asm-ex addr "c.lv: Invalid compressed register (use x8..x15).")))
    )

;; c.lvsp - load vyte from stack frame                 (lv rd offset(x2))
;; // rd ≠ x0
(def-ci lvsp
  "Compressed load vyte from stack pointer offset. Load rd with value from the
  memory address derived from adding value in the stack pointer to the
  8-bit immediate, a multiple of 4 zero extended to the register width.
  (lv rd x2 imm8) rd ≠ 0."
  [rd imm8]
  (let [addr (pc)]
    (if (cj-not (zero? (regno rd)))
         (hold-expr :c.lvsp [imm8]
                (cond
                  (cj-not (unsigned-int? imm8 8)) ;; Note: unsigned-int?!!
                  (asm-ex addr "c.lvsp: Immediate out of range.")
                  (cj-not (zero? (bit-and imm8 0x3)))
                  (asm-ex addr "c.lvsp: Immediate should be a multiple of 4")
                  :else
                  (build-rv-ins [3 1 5 3 2 2] 2 (bit-field imm8 5) (regno rd)
                                 (bit-field imm8 4 2) (bit-field imm8 7 6) 2)
           ))
    (asm-ex addr "c.lvsp: Can not use x0 as the destination register."))
    ))

;; c.svsp - store vyte to stack frame (sv r2 offset(x2))
(def-ci svsp
  "Compressed store vait to stack pointer offset: Store 32 bit value in rs2 to the
  memory address derived from adding the stack pointer (x2) to the 8-bit immediate,
  a multiple of 4 zero extended to the register width.
  (sv rs2 x2 imm)."
  [rs2 imm8]
  (let [addr (pc)]
   ;; (cimm imm8 rd 4 4)
     (hold-expr :c.svsp [imm8]
       (cond (cj-not (unsigned-int? imm8 8)) ;; Note: unsigned-int?!!
             (asm-ex addr "c.svsp Immediate out of range.")
             (cj-not (zero? (bit-and imm8 0x3)))
             (asm-ex addr "c.svsp Immediate should be a multiple of 4")
             :else
             (build-rv-ins [3 4 2 5 2] 6 (bit-field imm8 5 2)
                            (bit-field imm8 7 6) (regno rs2) 2)
              ))))

        ;;;; Miscelleneous instructions ;;;;

 ;ebreak
(def-ci ebreak []
  (build-rv-ins [4 5 5 2] 9 0 0 2))

        ;;;; Floating point instructions ;;;;

        ;;;; single floating point ;;;;

  ;; (c.flv       #b0110000000000000  0x6000  2  cl (frd rs1 imm6)) ;f //flv frd offset(rs1)
  ;; (c.fsv       #b1110000000000000  0xe000  2  cs (fr2 rs2 imm7))  ;f //fsv frs2 offset(rs1)
  ;; (c.flvsp     #b0110000000000010  0x6002  2  ci (frd imm6)) ;f //flv frd offset(x2)
  ;; (c.fsvsp     #b1110000000000010  0xe002  2  css (fr2 imm6)) ;f //fsv fr2 offset(x2)
  ;;

  ;; (c.flv       #b0110000000000000  0x6000  2  cl (frd rs1 imm6)) ;f //flv frd offset(rs1)
;; (c.flz       #b0010000000000000  0x2000  2  cl (frd rs1 imm6)) ;d //flz frd offset(rs1)
  ;; (c.fsv       #b1110000000000000  0xe000  2  cs (fr2 rs2 imm7))  ;f //fsv frs2 offset(rs1)
  ;; (c.fsz       #b1010000000000000  0xa000  2  cl (fr2 rs2 imm7))  ;d //fsz frs2 offset(rs1)

  ;; (c.flvsp     #b0110000000000010  0x6002  2  ci (frd imm6)) ;f //flv frd offset(x2)
  ;; (c.flzsp     #b0010000000000010  0x2002  2  ci (frd imm6)) ;d //fld frd offset(x2)
  ;; (c.fsvsp     #b1110000000000010  0xe002  2  css (fr2 imm6)) ;f //fsv fr2 offset(x2)
  ;; (c.fszsp     #b1010000000000010  0xa002  2  css (fr2 imm6)) ;d //fsd fr2 offset(x2)

