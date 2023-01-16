; Copyright (c) Kyuvi
; The use and distribution of this software are covered by the GNU General
; public license vesrion 3 (GPLv3, https://www.gnu.org/licenses/gpl-3.0.html).
; By using this software in any fashion you agree to be bound by this license.
; You may not remove this notice or any other from this software
;

(ns  jrv.korr.reg
    "Defines register record `RVRegister` for RISC-V registers
     and register functions.

       Integer Registers
     x0-31 = Basic Register Names
     zr(zero) = x0
     ra = Return Address = x1
     sp = Stack Pointer = x2
     gp = Global Pointer = x3
     tp = Thread Pointer = x4
     t0 = Temporary register and alternate link register = x5
     t1-2 = Temporaries = x6-7
     s0 = Saved Register = x8
     fp = Frame Pointer = x8
     s1 = Saved Register = x9
     a0-1 = Function arguments and return values = x10-11
     a2-7 = Function arguments = x12-17
     s2-11 = Saved Registers = x18-27
     t3-6 = Temporaries = x28-31

       Floating Point Registers
     f0-31 = Basic Register Names
     ft0-7 = Temporaries = f0-7
     fs1-2 = Saved Registers = f8-9
     fa0-1 = Function arguments and return values = f10-11
     fa2-7 = Function arguments = f12-17
     fs2-11 = Saved Registers = f18-27
     ft8-11 = Temporaries = f28-31 "
 {:author "Kyuvi"
  :license {:name "GPL-3.0 WITH Classpath-exception-2.0"
            :url "https://www.gnu.org/licenses/gpl-3.0.html"}}
  ;; not strictly neccesary, but might as well set a project wide standard
  ;; (:refer-clojure :rename {and cj-and, or cj-or, not cj-not, rem cj-rem})
  (:require [jrv.korr.cj-utils :refer :all])
  ;; (:refer jrv.korr.env )
  )

(defrecord RVRegister [f-reg reg-val c-val a-val t-val s-val])

(defn rv-reg? [obj] (= (type obj) RVRegister))

(defn f-reg? [obj] (and (rv-reg? obj) (:f-reg obj)))
(defn i-reg? [obj] (and (rv-reg? obj) (not (:f-reg obj))))
(defn e-reg? [obj] (and (rv-reg? obj) (not (:f-reg obj))
                                     (<= 0 (:reg-val obj) 15)))

(defn z-reg? [obj] (and (rv-reg? obj) (= (:reg-val obj) 0)))
(defn zr? [obj] (and (rv-reg? obj) (= (:reg-val obj) 0)))
(defn ra? [obj] (and (i-reg? obj) (= (:reg-val obj) 1)))
(defn sp? [obj] (and (i-reg? obj) (= (:reg-val obj) 2)))
(defn gp? [obj] (and (i-reg? obj) (= (:reg-val obj) 3)))
(defn tp? [obj] (and (i-reg? obj) (= (:reg-val obj) 4)))
(defn fp? [obj] (and (i-reg? obj) (= (:reg-val obj) 8)))

(defn c-reg? [obj] (and (rv-reg? obj) (:c-val obj)))
(defn a-reg? [obj] (and (rv-reg? obj) (:a-val obj)))
(defn s-reg? [obj] (and (rv-reg? obj) (:s-val obj)))
(defn t-reg? [obj] (and (rv-reg? obj) (:t-val obj)))

(defn ci-reg? [obj] (and  (i-reg? obj) (:c-val obj)))
(defn ai-reg? [obj] (and  (i-reg? obj) (:a-val obj)))
(defn si-reg? [obj] (and  (i-reg? obj) (:s-val obj)))
(defn ti-reg? [obj] (and  (i-reg? obj) (:t-val obj)))

(defn cf-reg? [obj] (and  (f-reg? obj) (:c-val obj)))
(defn af-reg? [obj] (and  (f-reg? obj) (:a-val obj)))
(defn sf-reg? [obj] (and  (f-reg? obj) (:s-val obj)))
(defn tf-reg? [obj] (and  (f-reg? obj) (:t-val obj)))

(defn regno
  "Extract the register number from `reg`."
  [reg]
  (assert (rv-reg? reg) ;;(= (type reg) RVRegister)
          (format "regno: %s not a RISC-V register." reg))
  (:reg-val reg))

(defn cregno [reg]
  "Extract the commpressed register number from `reg`."
  (let [cval (c-reg? reg)]
    (assert  cval ;;(and (rv-reg? reg) ;; (= (type reg) RVRegister) cval)
            (format "cregno: %s not a compressed instruction RISC-V register."
                    reg))
    cval))




(defmacro ^:private def-reg
  "Define the register names based on the value `regval` and the `float-pred`."
  [regval float-pred] ;TODO: pred
  (let [c-val (if (<= 8 regval 15) (bit-and regval 7) false)
        a-val (if (<= 10 regval 17) (- regval 10) false)
        s-val (cond (<= 8 regval 9) (bit-and regval 7)
                    (<= 18 regval 27) (bit-and regval 15)
                    :else false)
        t-val (cond (and (not float-pred) (<= 5 regval 7)) (- regval 5)
                    (and (not float-pred) (<= 28 regval 31)) (- regval 25)
                    (and float-pred (<= 0 regval 7))  regval
                    (and float-pred (<= 28 regval 31)) (- regval 20)
                    :else false)
        float-char (if float-pred \f nil)
        reg-sym (symbol (str (if float-char float-char \x) regval))
        ;; Alternate register names (recommended by standard)
        alt-val (or a-val s-val t-val)
        alt-char  (cond a-val \a s-val \s t-val \t :else nil)
        alt-sym (if alt-char (symbol (str float-char alt-char alt-val)) nil)
        alt-expr (if alt-sym `(def ~alt-sym ~reg-sym) nil) ;; TODO: add doc
        ;; Constant register names (recommended by standard)
        cons-sym (case regval 0 'zr 1 'ra  2 'sp 3 'gp 4 'tp 8 'fp  nil)
        cons-expr (if (and (not float-pred) cons-sym)
                      `(def ~cons-sym ~reg-sym) nil) ;; TODO: add doc
                    ]
  `(do
    (def ~reg-sym ;; TODO: add doc
      ;; (str "Register " ~reg-sym " of the RISC-V instruction set architecture
            ;; alternativly known as" ~alt-sym
            ;; (if ~cons-sym (str "or " ~cons-sym) nil ) "." )
          (->RVRegister ~float-pred ~regval ~c-val ~a-val ~t-val ~s-val) )
    ~alt-expr
    ~cons-expr
   )   )  )





(defn build-registry
  "Build registers based on the `arch`.
  `arch` can be one of :e :float or :int "
  [arch]
  (dotimes [v (if (= arch :e) 16 32)]
      (eval `(do ~(when (= arch :float) `(def-reg ~v true))
                      (def-reg ~v false))))  ; )
)

(build-registry :i)
