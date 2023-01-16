(ns jrv.korr.fmt
  "Instruction formats for the jrv RISC-V assembler."
   {:author "Kyuvi"
    :license {:name "GPL-3.0 WITH Classpath-exception-2.0"
              :url "https://www.gnu.org/licenses/gpl-3.0.html"}}
  (:require [jrv.korr
             [cj-utils :refer :all]
             [env :refer :all]
             [reg :refer :all]
            ])
  ;; (:refer jrv.korr.env)
  ;; (:refer jrv.korr.reg)
  )


(defn build-rv-ins
  "Build a 16 or 32-bit RISC-V machine code instruction to be emitted into the
  code-vector."
  [bit-width-vect & args]
  (assert (= (count bit-width-vect) (count args))
          "build-rv-ins: bit-width and args are unequal")
;; (bit-shift-right (first bit-width-vect) (first args))
  (loop [ins-vait 0 width-vector bit-width-vect value-list args]
    (let [width (first width-vector) value (first value-list)]
      (if-not width
        ins-vait
        ;; (list ins-vait width value)
        ;; (bit-shift-right  ins-vait width); value)
        (do
          (assert (zero? (bit-shift-right value width))
                  (format
                   "Build-rv-ins: value: %X does not fit into width: %X."
                   ;; "Build expressions value: %X and width: %X at %X do not fit."
                   value width) ; (pc))
                  )
          (recur (bit-or (bit-shift-left ins-vait width) value)
                 (next width-vector)
                 (next value-list)))
        ))))

        ;;;; RISC-V Commpressed module instruction formats ;;;;


(defn creg
  "Compressed Register: RISC-V compressed 'C' module instruction format for
  instructions that use two x0..x31 registers."
  [rd rs1 op4 op2]
  (build-rv-ins [4 5 5 2] op4 (regno rd) (regno rs1) op2))

(defn cimm
  "Compressed Immediate: RISC-V compressed 'C' module instruction format for
  instructions that use an x0..x31 register and a 6-bit signed immediate."
  [imm6 rd op1 op2]
  (let [addr (pc)]
    (hold-expr :cimm [imm6]
      (if (signed-int? imm6 6)
        (build-rv-ins [3 1 5 5 2] op1 (bit-field imm6 5) (regno rd)
                                      (bit-field imm6 4 0) op2)
        (asm-ex  addr "cimm: Immediate %s value out of range." imm6))
   )))

(defn ciwid
  "Compressed Wide Immediate: RISC-V compressed 'C' module instruction format
  for instructions that use a c-register (x8..x15) and an 8-bit signed immediate."
  [imm8 crd op1 op2]
  (let [addr (pc)]
    (hold-expr :ciwid [imm8]
      (if (signed-int? imm8 8)
        (build-rv-ins [3 2 4 1 1 3 2] op1 (bit-field imm8 7 6)
                                       (bit-field imm8 5 2) (bit-field imm8 0 )
                                       (bit-field imm8 1) (cregno crd) op2);)
        (asm-ex addr "ciwid: Immediate value out of range."))
      )))

;; cs - Compressed load and Store
;; (defn cldst
;;   [imm7 cr1 cr2 op1 op2]
;;   (let [addr (pc)]
;;     (hold-expr :cldst [imm7]
;;       (cond (not (signed-int? imm7 5))
;;             (asm-ex addr "cldst: immediate value out of range.")
;;             (not (zero? (bit-and imm7 3)))
;;             (asm-ex addr "cldst: immediate '%s' should be a multiple of 4" imm7)
;;             :else
;;             (build-rv-ins [3 3 3 1 1 3 2] op1 (bit-field imm7 5 3) (cregno cr2)
;;                                           (bit-field imm7 2) (bit-field imm7 6)
;;                                           (cregno cr1) op2)
;;       )))

(defn cldst
  "Compressed load and store: Base of the compressed istsuctuctions that load
  and store to memory. Note: does not do any checking!"
  [imm3 imm2 cr1 cr2 op1 op2]
  (build-rv-ins [3 3 3 2 3 2] op1 imm3 (cregno cr2) imm2 (cregno cr1) op2)
  )

  ;; css - Compressed Stack Relative Store
(defn cstrl
  "Compressed stack relative (load and store): Base of the compressed
  intructuctions that load and store to memory relative to the stack pointer
  (sp/x2). Note: does not do any checking!"
  [imm6 rs2 op1 op2]
  (build-rv-ins [3 6 5 2] op1 imm6 (regno rs2) op2);)
  )


(defn carith
 "RISC-V compressed 'C' module instruction format for instructions that
  use 2 'c-register' (x8..x15)."
  [op3 op1 op2 crd op2b crs2]
        (build-rv-ins [3 1 2 3 2 3 2] op3 op1 op2 (cregno crd) op2b
                                       (cregno crs2) 1)
   )

(defn cjump
 "RISC-V compressed 'C' module instruction format for jump instructions that
  take only signed immediate arguments.
  Note: This function calculates and uses the offset from the given immediate
  (label)."
  [imm funct3 op]
  (let [addr (pc) ofst (offset imm)]
    (hold-expr :cjump [ofst] ;(imm)
      (cond (not (signed-int? ofst 12))
            (asm-ex addr "cjump: Immediate value out of range.")
            (not (zero? (bit-and ofst 0x1)))
            (asm-ex addr "cjump: Immediate value should be a multiple of 2.")
            :else
            (build-rv-ins [3 1 1 2 1 1 1 3 1 2]
                          funct3 (bit-field ofst 11) (bit-field ofst 4)
                          (bit-field ofst 9 8) (bit-field ofst 10)
                          (bit-field ofst 6) (bit-field ofst 7)
                          (bit-field ofst 3 1)(bit-field ofst 5) op)
        ))))

(defn cbranch
  "Compressed Branch: RISC-V compressed 'C' module instruction format for
  branch instructions that take a 'c' register (x8..x15) and an immediate
  as arguments.
  Note: This function calculates and uses the offset from the given immediate
  (label)."
  [imm crs1 funct3 op]
  (let [addr (pc) ofst (offset imm)]; addr)))
    (hold-expr :cbranch [ofst]
      (cond (not (signed-int? ofst 9))
            (asm-ex addr "cbranch: Immediate value out of range.")
            (not (zero? (bit-and ofst 0x1)))
            (asm-ex addr "cbranch: Immediate value should be a multiple of 2.")
            :else
            (build-rv-ins [3 1 2 3 2 2 1 2]
                          funct3 (bit-field ofst 8) (bit-field ofst 4 3)
                          (cregno crs1) (bit-field ofst 7 6)
                          (bit-field ofst 2 1) (bit-field ofst 5) op)
             ))))

        ;;;; RISC-V Base instruction formats  ;;;;

(defn register
 "RISC-V base 'I' module instruction format for instructions that
  use three registers."
  [funct7 rs2 rs1 funct3 rd op]
    (build-rv-ins [7 5 5 3 5 7] funct7 (regno rs2) (regno rs1) funct3
                                    (regno rd) op))


(defn immed
 "RISC-V base 'I' module instruction format for instructions that
  use two registers and a 12-bit signed immediate 'imm12'."
  [imm12 rs1 funct3 rd op]
  (let [addr (pc)]
  (hold-expr :immed [imm12]
      (if (signed-int? imm12 12)
        (build-rv-ins [12 5 3 5 7] ;; imm12
                                      (bit-and imm12 0xfff)
                                      (regno rs1) funct3 (regno rd) op)
        (asm-ex addr "Immed: Immediate value out of range."))
      )))

(defn branch
 "RISC-V base 'I' module instruction format for branch instructions that
  compare rs1 and rs2 and jump to signed immedate (label) 'imm'.
  Note: This function calculates and uses the offset from the given immediate
  (label)."
  [imm rs2 rs1 funct3 op]
  (let [addr (pc) ofst (offset imm)]
    (hold-expr :branch [ofst] ; (imm)
        (cond (not (signed-int? ofst 13))
              (asm-ex addr "Branch: Immediate value out of range.")
              (not (zero? (bit-and ofst 1)))
               (asm-ex addr "Branch: Immediate should be a multiple of 2.")
             :else
               (build-rv-ins [1 6 5 5 3 4 1 7]
                             (bit-field ofst 12) (bit-field ofst 10 5)
                             (regno rs2) (regno rs1) funct3 (bit-field ofst 4 1)
                             (bit-field ofst 11) op)
               ))))


(defn jump
 "RISC-V base 'I' module instruction format for jump and link instructions that
  store the return address in rd and jump to signed immediate (label) 'imm'.
  Note: This function calculates and uses the offset from the given immediate
  (label)."
  [imm rd]
  (let [addr (pc) ofst (offset imm)]
    (hold-expr :jump [ofst] ;(imm)
        (cond (not (signed-int? ofst 21))
              (asm-ex addr "Jump: Immediate value out of range.")
              (not (zero? (bit-and ofst 1)))
              (asm-ex addr "Jump: Immediate should be a multiple of 2.")
              :else
              (build-rv-ins [1 10 1 8 5 7]
                            (bit-field ofst 20) (bit-field ofst 10 1)
                            (bit-field ofst 11) (bit-field ofst 19 12)
                            (regno rd) 0x6f)
               ))))



(defn store
  "RISC-V base 'I' module instruction format for instructions that store the
  contents of the address created by adding the contents of the 'base' register
  to the 12-bit signed immediate 'imm12' into the 'src' register."
  [imm12 src base funct3]
  (let [addr (pc)]
    (hold-expr :store [imm12]
      (if (signed-int? imm12 12)
        (build-rv-ins [7 5 5 3 5 7] (bit-field imm12 11 5) (regno src) (regno base)
                                    funct3 (bit-field imm12 4 0) 0x23)
        (asm-ex addr "Store: Immediate value out of range."))
      )))


(defn upperimm
 "RISC-V base 'I' module format for function that load 20-bit upper immediate
  into register rd.
  This funciton expects a 32-bit signed immediate that is a multiple of
  0x1000(4096)."
  [imm32 rd op]
  (let [addr (pc)]
    (hold-expr :upperimm [imm32]
      (cond (not (signed-int? imm32 32))
            (asm-ex addr "Upper Immediate value out of range.")
            (not (zero? (bit-and imm32 0xfff )))
            (asm-ex addr "Upper Immediate should be a multiple of 0xfff(4096).")
            :else (build-rv-ins [20 5 7] (bit-field imm32 31 12) (regno rd) op)
             ))))

        ;;;; Multiplication/Division instruction formats ;;;;

(defn muldiv
 "RISC-V format for multiplication and division instructions in
  Integer multiplication and division 'M' module."
  [rs2 rs1 funct3 rd op]
    (build-rv-ins [7 5 5 3 5 7] 1 (regno rs2) (regno rs1) funct3
                                      (regno rd) op))


        ;;;; Control and Status Register formats ;;;;

(defn csrreg
 "RISC-V Control and Status Register 'ZiCSR' module format for CSR instructions
  that use registers."
  [csr rs1 funct3 rd]
  (let [addr (pc)]
    (hold-expr :csr [csr]
      (if (unsigned-int? csr 12)
          (build-rv-ins [12 5 3 5 7] csr (regno rs1) funct3 (regno rd) 0x73)
          (asm-ex addr "csr: Wrong control and status register address"))
      )))


(defn csrimm
 "RISC-V Control and Status Register 'ZiCSR' module format for CSR instructions
  that use 5-bit unsigned immediates."
  [csr uimm5 funct3 rd]
  (let [addr (pc)]
    (hold-expr :csri [uimm5 csr]
      (cond (not (unsigned-int? uimm5 5))
            (asm-ex addr "csri: Immediate value out of range.")
            (not (unsigned-int? csr 12))
            (asm-ex addr "csri: Wrong control and status register address")
            :else (build-rv-ins [12 5 3 5 7] csr uimm5 funct3 (regno rd) 0x73))
     )))


;; (defn get-string-args (decl)
;;   (let [docstr (if (= (type (first decl) str)) (first decl))
;;         argvec (if docstr (second decl) (first decl))]
;;     (list docstr argvec)


(defmacro def-ci
  [nam & fdecl]
  (let [proto-name  (symbol-append '% nam )
        docstr (if (= (type (first fdecl)) String) (first fdecl))
        argvec (if docstr (second fdecl) (first fdecl))
        proto-def `(defn ~proto-name ~@fdecl)
        emit-def  (if docstr
                   `(defn ~nam ~docstr ~argvec
                      (emit-kait! (~proto-name ~@argvec)))
                   `(defn ~nam ~argvec
                      (emit-kait! (~proto-name ~@argvec))))]
    ;; `(defn (symbol-append '% ~nam ) & ~@fdecl)
    ;; (if docstr
      ;; `(defn ~nam ~docstr ~argvec
      ;;    (emit-kait! ((symbol-append '% ~nam) ~@argvec)))
      ;; `(defn ~nam ~argvec
      ;;    (emit-kait! ((symbol-append '% ~nam) ~@argvec)))
    `(do
      ~proto-def
      ~emit-def
      )))


;; (def-ci nop  "test" [ rd ] (build-rv-ins [16] 1))


(defmacro def-vi
  [nam & fdecl]
  (let [proto-name  (symbol-append '% nam )
        docstr (if (= (type (first fdecl)) String) (first fdecl))
        argvec (if docstr (second fdecl) (first fdecl))
        proto-def `(defn ~proto-name ~@fdecl)
        emit-def  (if docstr
                   `(defn ~nam ~docstr ~argvec
                      (emit-vait! (~proto-name ~@argvec)))
                   `(defn ~nam ~argvec
                      (emit-vait! (~proto-name  ~@argvec))))]
    `(do
      ~proto-def
      ~emit-def
      )))
