(ns jrv.ins.zicsr
  "Control and Status Instructions for the jrv RISC-V assembler."
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

        ;;;; Register instructions ;;;;

(def-vi csrrw
  "Reads contents of csr (zero extended) into rd and writes contents of rs1 to the csr.
  If rd = x0 does not read and only writes to csr."
  [rd csr rs1]
  (csrreg csr rs1 1 rd))

(def-vi csrrs
  "Reads contents of csr (zero extended) into rd and sets the bits in the csr
  that are set in rs1. If rs1 = x0 does not write and only reads from csr."
  [rd csr rs1]
  (csrreg csr rs1 2 rd))

(def-vi csrrc
  "Reads contents of csr (zero extended) into rd and clears the bits in the csr
  that are set in rs1. If rs1 = x0 does not write and only reads from csr."
  [rd csr rs1]
  (csrreg csr rs1 3 rd))

        ;;;; Immediate instructions ;;;;

(def-vi csrrwi
  "Reads contents of csr into rd and writes zero extended immediate 'uimm5' to the csr.
  If rd = x0 does not read and only writes to csr."
  [rd csr uimm5]
  (csrimm csr uimm5 5 rd))

(def-vi csrrsi
  "Reads contents of csr (zero extended) into rd and sets the bits in the csr
  that are set in the immediate 'uimm5'.
  If rd = x0 does not read and only writes to csr."
  [rd csr uimm5]
  (csrimm csr uimm5 6 rd))

(def-vi csrrci
  "Reads contents of csr (zero extended) into rd and clears the bits in the csr
  that are set in the immediate 'uimm5'.
  If rd = x0 does not read and only writes to csr."
  [rd csr uimm5]
  (csrimm csr uimm5 7 rd))

        ;;;; Derived register instructions ;;;;

(defn csrr
  "Reads contents of csr (zero extended) to rd."
  [rd csr]
  (csrrs rd csr x0))

(defn csrw
  "Write contents of rs to csr."
  [csr rs]
  (csrrw x0 csr rs))

(defn csrs
  "Sets the bits in the csr that are set in rs."
  [csr rs]
  (csrrs x0 csr rs))

(defn csrc
  "Clears the bits in the csr that are set in rs."
  [csr rs]
  (csrrc x0 csr rs))

        ;;;;  Derived immediate instructions  ;;;;

(defn csrwi
  "Write zero extended immediate 'uimm5' to csr."
  [csr uimm5]
  (csrrwi x0 csr uimm5))

(defn csrsi
  "Sets the bits in the csr that are set in the immediate 'uimm5'."
  [csr uimm5]
  (csrrsi x0 csr uimm5))

(defn csrci
  "Clears the bits in the csr that are set in the immediate 'uimm5'."
  [csr uimm5]
  (csrrci x0 csr uimm5))
