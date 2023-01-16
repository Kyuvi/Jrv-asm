(ns jrv.ins.priv
  "Privilaged Architecture Instructions for the jrv RISC-V assembler."
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

(def-vi uret
 "Return to user mode from trap.
  Sets the pointer counter to the value stored in the uepc register."
  []
  (build-expr-code '(16 16) #x20 #x73))

(def-vi sret
 "Return to Supervisor mode from trap.
  Sets the pointer counter to the value stored in the sepc register."
  []
  (build-expr-code '(16 16) #x1020 #x73))

(def-vi mret
 "Return to Machine mode from trap.
  Sets the pointer counter to the value stored in the mepc register."
  []
  (build-expr-code '(16 16) #x3020 #x73))

(def-vi wfi
 "Wait for interrupt.
  Provides a hint to the implementation that the current hart can be stalled
  until an interrupt might need servicing"
  []
  (build-expr-code '(16 16) #x1050 #x73))

(def-vi sfence.vma
 "(sfence.vma)"
  [rs1 rs2]
  (build-expr-code '(7 5 5 3 12) #x9 rs2 rs1 #x0 #x73))

(def-vi ebreak
 "(i.ebreak)"
  []
  (build-expr-code '(16 16) #x10 #x73)) ;c
