(ns jrv.korr.env
  "Environment setup for the jrv RISC-V assembler."
  {:author "Kyuvi"
   :license {:name "GPL-3.0 WITH Classpath-exception-2.0"
            :url "https://www.gnu.org/licenses/gpl-3.0.html"}}
  ;; not strictly neccesary, but might as well set a project wide standard
  ;; (:refer-clojure :rename {and cj-and, or cj-or, not cj-not, rem cj-rem})
  (:require [jrv.korr.cj-utils :refer :all] )
  )

;; The assembler outputs a byte file this record contains the final vector
;; that is output and the byte address of the final byte in that vector
(defrecord CodeVector [env-code-vec env-addr])

;; depreciated code
;; ;; Jrv provides both a label table and a type table instead of a
;; ;; single symbol table it also uses a field-map for Kozo (structure) fields.

;; ;; As most assembly code contains labels to jump to, this record provides
;; ;; a (hash-map) lookup table of the labels in the code and their
;; ;; descriptions.
;; ;; (defrecord LabelTable [env-sym-table])


;; ;; This assembler should have types this is a hash map of type names and their
;; ;; descriptions
;; ;; (defrecord TypeTable [env-type-table])

;; Jrv provides a SymbolTable  record containing two (atom) hashmaps,
;; a label table and a type table.
;; As most assembly code contains labels to jump to, the env-label-table hashmap
;; provides a  lookup table of the labels in the code and their descriptions.
;; This assembler should have types, the env-type-table is a hash map of type
;; names and their descriptions
(defrecord SymbolTable [^clojure.lang.PersistentArrayMap env-label-table
                        ^clojure.lang.PersistentArrayMap env-type-table
                        ^clojure.lang.PersistentArrayMap builtin ])

        ;;;; Descriptions ;;;;

;; The labels themselves will be described with by a record containing relevant
;; data
(defrecord LabelDsc [label-val
                     label-class  ; ptr var field param type Sproc Sfunc Mod
                     rv-type
                     rdo ;; read only
                     rv-doc])


;; The types will also be described with by a record containing relevant data
;; it also has a field-map for Kozo (structure) field descriptions.
(defrecord TypeDsc [base-type length arr-base field-map size rv-doc])


;; If the type is a "Kozo" (structure ) it will have fields whose data will be
;; described by the record `field-dsc`
(defrecord FieldDsc [offset rv-type
                      ;; f-size
                      rv-doc] )

        ;;;; Basic Environment ;;;;

;; With those defined a record of the basic environment can now be defined
(defrecord BasicEnv [^CodeVector code-vec
                      ;; ^LabelTable symbol-table
                     ;; ^TypeTable type-table
                     ^SymbolTable sym-table
                     ;; ^map builtin
                     ]
  )

(def jrv-builtin-types
  ;; TODO: unsigned?
  ;; TODO: array? kozo?
  ;; TODO: TypeDsc format?
  "Map containing jrv builtin types and sizes (in bytes)
   :flag, :nibble, :byte, :kait, :vait, :zait, :yait"
  {:flag 0.125 :nibble 0.5 ;; for reference only
   :byte 1 :kait 2 :vait 4 :zait 8 :yait 16
   :b 1 :k 2 :v 4 :z 8 :y 16
   ;; 'flag 0.125 'nibble 0.5 ;; for reference only
   ;; 'byte 1 'kait 2 'vait 4 'zait 8 'yait 16
   })


        ;;;; Construcors ;;;;

(defn make-code-vector
  "Create a new CodeVector record with an initial address of `address`."
  ([] (make-code-vector 0))
  ([address] (->CodeVector (atom []) (atom address))))

(defn make-sym-table
  "Create a new SymbolTable record including the builtin rv types if
  `builtin-pred`is true."
  [builtin-pred]
  (->SymbolTable (atom {}) (atom {}) (if builtin-pred jrv-builtin-types {})))


(defn make-basic-env
  "Create a new BasicEnv record with an initial address of `address`.
  `address` defaults to 0."
  ([] (make-basic-env 0))
  ([address]
   (def ^:const origin address)
   (->BasicEnv (make-code-vector)
               (make-sym-table true)
               ;; jrv-builtin-types
               )
   ))


        ;;;; Future Labels ;;;;


;; The placeholder for the value field of the Placeholder type
(def ^:dynamic *lazy-marker* :postponed)

(defrecord Placeholder [name fun value])

(defn make-placeholder
  "Create a new Placeholder record with :name `nam` and :fun `fun`
  and making :value an atom of val `value`.
  `value` defaults to *lazy-marker*. "
  ([nam fun]
  (->Placeholder nam fun (atom *lazy-marker*)))
  ([nam fun value]
   (->Placeholder nam fun (atom value))))

(defn placeholder?
  "Test if an object is a `Placeholder` record."
  [obj]
  (= (type obj) Placeholder))

(defn ^:private set-placeholder-value! [placeholder value]
  (reset! (:value placeholder) value))


;; (defn place-exception [msg placeholder]
;;   (ex-info msg {:place-name (:pname placeholder)}))
(defn placeholder-value-exception
  ([] (placeholder-value-exception nil))
  ([state]
   (ex-info (format "Placeholder Exceptiontion: %s" state)
            {:state state :type :placeholder-value })))

;; (defmulti resolve-placeholder (fn ([x] (type x)) ([x pred] (type x) y)))

;; (defmethod resolve-placeholder Long
;;   ([x] x)
;;   ([x pred] x))

;; (defprotocol PlaceholderProtocol
(defprotocol ResolvePlaceholder
  (resolve-placeholder [x] [x pred]
    "If x is an integer returns x, otherwise
     Forces computation of an expression held in a `Placeholder` record."))

;; (extend-protocol PlaceholderProtocol
(extend-protocol ResolvePlaceholder
  ;; java.lang.Long
  Long
  (resolve-placeholder
    ([x] x)
    ([x err-pred] x))
  clojure.lang.BigInt
  (resolve-placeholder
    ([x] x)
    ([x err-pred] x))
  BigInteger
  (resolve-placeholder
    ([x] x)
    ([x err-pred] x))
  Placeholder
  (resolve-placeholder
    ([p] (resolve-placeholder (p true)))
    ([p err-pred]
    (if-not (identical? @(:value p) *lazy-marker*)
      @(:value p)
      (try (set-placeholder-value! p ((:fun p)))
           (catch clojure.lang.ExceptionInfo e
             (if (or (not (= (:type (ex-data e) :placeholder-value)))
                      err-pred)
               (throw e)
               e)
             ;; ( (if err-pred (throw e) e))
             p)
           ;; (finally p)
           )))
  ))


(defn ^:private parse-binding [spec]
  (cond
    (symbol? spec) (list spec spec)
    (and (list? spec) (= (count spec) 2)) spec
    ;; :else (threxf (format "parse-binding: Spec `%s` not recognised" spec))))
    :else (threxf "parse-binding: Spec `%s` not recognised" spec)))

(defmacro resolving-placeholder [dependency-vector & body]
  (assert (vector dependency-vector)
          (format "resolving-placeholder: %s is not a vector" dependency-vector))
  (let [bindings (map parse-binding dependency-vector)]
    `((fn ~(vec (map first bindings)) ~@body)
      ;; ~@ ;;TODO:
      ~@(map (fn [b] `(force ~(second b))) bindings)
      )))

(defmacro hold-expr
  "Evaluates `body`if possible, otherwise
   Stores the expresion `body` in a `Placeholder` record of name `nam`
   and returns the `Placeholder`."
  [nam dependency-vector & body]
  (assert (vector? dependency-vector)
          (format "hold-expr: %s is not a vector" dependency-vector))
  `(resolve-placeholder
    (make-placeholder ~nam
                      (fn [] (resolving-placeholder ~dependency-vector ~@body)))
    nil))

(defn resolve-tree [tree]
  (cond
    (list? tree) (list (resolve-tree (first tree))
                       (resolve-tree (rest tree)))
    (nil? tree) tree
    (integer? tree ) tree
    (placeholder? tree) (resolve-placeholder tree)))

(defn ^:private resolve-vector-loop [vect]
  "Helper function for resolve-vector"
  (loop [out-vec [] problems [] loop-vector vect]
    (let [x (first loop-vector)]
      (if-not x
        [out-vec problems]
        (let [position-result
              (try (resolve-placeholder x true)
                   (catch clojure.lang.ExceptionInfo e
                     (list x (:state (ex-data e)))))]
          (if (list? position-result)
            (recur (conj out-vec (first position-result))
                   (conj problems (second position-result))
                   (next loop-vector))
            (recur (conj out-vec position-result)
                   problems
                   (next loop-vector))
        ))))))

(defn resolve-vector [vect]
  (let [temp-vec (resolve-vector-loop vect)
        out-vec (first temp-vec)
        problems (second temp-vec)]
    (if (empty? problems)
      out-vec
      (threxf "Unable to resolve output due to the following:\n%s\n" problems)
      )))

;; (defn resolve-vector [vect]
;;   ;; TODO:
;;   (let [temp-vec
;;         (loop [out-vec [] problems [] x (first vect)]
;;           (if-not x
;;             [out-vec problems]
;;             (try (recur (conj out-vec (resolve-placeholder x true))
;;                         problems (next vect))
;;                  (catch clojure.lang.ExceptionInfo e
;;                    (recur (conj out-vec x)
;;                           (conj problems (:state (ex-data e)))
;;                           (next vect))))))
;;         ;; (map (fn [x] (try (resolve-placheholder x true)
;;         ;; (catch clojure.lang.ExceptionInfo e ))))
;;         out-vec (first temp-vec)
;;         problems (second temp-vec)]
;;   (if (empty? problems)
;;     out-vec
;;     (threxf "Unable to resolve output due to the following:\n%s\n" problems)
;;   ))
;;
;; (defn pass-pending [obj]
;;     (if (pend-type? obj)
;;         (if (realized? obj) @obj
;;             (future @obj))
;;             obj))



        ;;;; Bits, bytes, kaits, vaits and yaits  ;;;;
;;
;; The instruction are loaded into the code-vector in a series of bytes (8-bits)
;; Risc-v uses EA-tip (Egyptian/Arabic tip-to-memory) addressing,
;; also known as little endian, this means the 32 or 16 bit instructions/data
;; are inverted/reversed bytewise (in 8-bit chunks) before being stored into
;; memory



(defn test-rv-size
  "Test if `x` is of the basic RISC-V size `size-key`.
   `size-key` should be one of :byte, :kait, :vait, :zait or yait."
  [x size-key]
  (let [size-map {:byte 8 :kait 16 :vait 32 :zait 64 :yait 128}
        size (get size-map size-key)
        ;; size-no (size size-map)
        ]
  (assert  size
          (format "test-rv-size: `size-key` %s should be one of the keywords
                   :byte, :kait, :vait, :zait or yait"
                  x size-key))
  (hold-expr (keyword-append size-key :test) [x]
               (if (unsigned-int? x size)
                 x
                 (threxf "%s does not fit into %s" x size-key)))))

(defprotocol GetByte
(get-byte [value byte-pos]
    "Extract the byte at position `byte-pos` from `value`")
  )

;; (extend-protocol PlaceholderProtocol
(extend-protocol GetByte
  ;; java.lang.Long
  Long
  (get-byte [value byte-pos]
    (bit-field value (* byte-pos 8) (dec (* (inc byte-pos) 8)))) ;; TODO:
  clojure.lang.BigInt
  (get-byte [value byte-pos]
    (bit-field value (* byte-pos 8) (dec (* (inc byte-pos) 8))))
  BigInteger
  (get-byte [value byte-pos]
    (bit-field value (* byte-pos 8) (dec (* (inc byte-pos) 8))))
  Placeholder
  (get-byte [value byte-pos]
    (hold-expr :bait [value] (get-byte value byte-pos)))
  )


(defn byte-rev
  "Returns a vector of the bytes of `obj` in revese order.
  The size of the vector (number of bytes) returned depends on `size-key`,
  which should be one of :byte :kait :vait :zait :yait."
  [size-key obj]  ; byte-pos-max]
  (let [max-byte-pos-map {:byte 0 :kait 1 :vait 3 :zait 7 :yait 15}
        max-byte-pos (get max-byte-pos-map size-key)]
    (assert max-byte-pos
            (format "byte-rev: Argument %s is not one of the keywords
             :byte :kait :vait :zait :yait" size-key ))
  (loop [return-vec [] byte-pos 0]
    (if (> byte-pos max-byte-pos)
      return-vec
      (recur (conj return-vec
                   (hold-expr size-key [obj]
                                (get-byte (test-rv-size obj size-key)
                                          byte-pos)))
             (inc byte-pos))))))


;; (defn rev-byte [byte]
;;   (byte-rev [:byte byte]))

;; (defn rev-kait [kait]
;;   (byte-rev [:kait kait]))

;; (defn rev-vait [vait]
;;   (byte-rev [:vait vait]))

;; (defn rev-zait [zait]
;;   (byte-rev [:zait zait]))

;; (defn rev-yait [yait]
;;   (byte-rev [:yait yait]))

(defn join-masks [x y]
  (let [big-x (biginteger x) big-y (biginteger y)]
  (if (zero? (.and big-x big-y))
    (.or big-x big-y )
    (threxf "Bitmasks %s and %s overlap!" x y))))



                    ;;;;;;;;  Environment functions  ;;;;;;;;
(declare ^:dynamic *max-address*)

(defprotocol EnvironmentProtocol
  (env-emit! [env vect]
    "Emit the vector `vect` of bytes into the assembly environment `env`.")
  (env-address [env]
    "Returns the current virtual address of the environment `env`.")
  (env-set-address [env address]
    "Sets the current virtual address of the environment.")
  (env-find-label [env sym]
    "Returns the description of a label within the environment `env`.")
  (env-set-label! [env sym label-class rv-type read-only rv-doc]
                 [env sym label-class rv-type read-only rv-doc address]
    "Creates a description of label `sym` within the environment `env`.
     If address is not supplied, the current address is used.")
  (env-set-parent-label! [env sym label-class rv-type read-only rv-doc]
                        [env sym label-class rv-type read-only rv-doc address]
    "Creates a description of label `sym` within the parent environment of `env`.
     If address is not supplied, the current address is used")
  (env-get-label-addr [env sym]
    "Returns the address of a label within the environment `env`.")
  (env-set-label-addr! [env sym] [env sym address]
    "Sets the address of a label within the environment `env`.
     If address is not supplied, the current address from the CodeVetcor is used")
  (env-find-type [env sym]
    "Returns the description (or size if a builtin type) of the rv-type `sym`.")
  (env-set-type! [env sym base len arr-base field-map size rv-doc ]
    "Creates a description for the TypeDsc `sym` within the environment `env`.")
  (env-set-parent-type! [env sym base len arr-base field-map size rv-doc]
    "Creates a description for the TypeDsc `sym` within the parent environmnmt of `env`.")
  (env-emit-ins [env vect]
     "Emit an instruction into the assembly environment. This is a
    hint, for environments which want to handle instructions specially.")
  (link [env]
    "Prepare and return final assembled output vector."))


(extend-protocol EnvironmentProtocol
  CodeVector
  (env-address [code-vec]
     @(:env-addr code-vec ))
  (env-emit! [code-vec vect]
    (let [addr (env-address code-vec) vect-len (count vect)]
      (when (> (+ addr vect-len) *max-address*)
        (printf
         "Warning: Content emit of %X bytes at %X will overflow address space"
         vect-len addr))
      (doseq [x vect]
        ;; (fn [x]
          (if-not (or (placeholder? x) (unsigned-int? x 8))
                  (threxf "Attempt to emit gabage (%s) at %X"
                          x addr))
          (swap! (:env-code-vec code-vec) conj x))
      ;; )
      (swap! (:env-addr code-vec) + vect-len )))
  (link [code-vec]
    (resolve-vector @(:env-code-vec code-vec)))

  SymbolTable
  (env-find-label [table sym]
    (let [kii (keyword sym)]
      (get @(:env-label-table table) kii))
    )
  (env-set-label! [table sym label-class rv-type rdo rv-doc addr]
    (let [kii (keyword sym)]
      (swap! (:env-label-table table)
             assoc kii (->LabelDsc addr label-class rv-type rdo rv-doc))))
  ;; (env-get-label-addr [env sym])
  ;; (env-set-label-addr [env sym] [env sym address])
  (env-find-type [table sym]
    (let [kii (keyword sym)]
      (or (get (:builtins table) kii) (get @(:env-type-table table) kii)))
    )
  (env-set-type! [table sym base len arr-base field-map size rv-doc]
    (let [kii (keyword sym)]
      (if (contains? (:builtins table) kii)
        (threxf "env-set-table: Cannot redefine builtin type %s" sym)
        (swap! (:env-type-table table)
               assoc kii (->TypeDsc base len arr-base field-map size rv-doc)))))

  BasicEnv
  (env-emit! [env vect]
    (env-emit! (:code-vec env) vect))
  (env-address [env]
    (env-address (:code-vec env)))
  (env-find-label [env sym]
    (env-find-label (:sym-table env) sym))
  (env-set-label!
    ([env sym label-class rv-type rdo rv-doc]
     (env-set-label! (:sym-table env) sym label-class rv-type rdo rv-doc
                    (env-address (:code-vec env))))
    ([env sym label-class rv-type rdo rv-doc addr]
     (env-set-label! (:sym-table env) sym label-class rv-type rdo rv-doc addr)))
  ;; (env-get-label-addr [env sym])
  ;; (env-set-label-addr [env sym] [env sym address])
    (env-find-type [env sym]
      (env-find-type (:sym-table env) sym))
    (env-set-type! [env sym base len arr-base field-map size rv-doc ]
      (env-set-type! (:sym-table env) sym base len arr-base field-map size rv-doc ))
    (env-emit-ins [env vect] (env-emit! env vect ))
    (link [env]
         (resolve-vector (:code-vec env ))) )
;; )



        ;;;; Local environments ;;;;

;; Local environmet, the base upon which to build local symbol scopes
;; and special-purpose environments
;;
;; A local environment consists of a parent environment
;; (containing a code-vector and a symbol table ) and a local symbol table that is
;; used for symbols defined within the local environment, so any symbols in
;; the local symbol table can not be accesed from the parent environment. Also
;; the  local symbols  are searched first so if a symbol exists in both environments,
;; the one in the local environment is prefered over (shadows) the one in the parent
;; environment
;;
;;

(defrecord LocalEnv [env-parent ^SymbolTable local-sym-table] ;)
  EnvironmentProtocol
;; TODO: move protocols inside defrecord?
;; (extend-protocol EnvironmentProtocol
  ;; LocalEnv
  (env-emit! [env vect] (env-emit! (:env-parent env ) vect))

  (env-address [env] (env-address (:env-parent env)))

  (env-set-address [env address]
    (reset! (:env-addr (:code-vec (:env-parent env))) address)
    )
  (env-find-label [env sym]
    (let [label-dsc (env-find-label (:local-sym-table env) sym)]
      (if  label-dsc
        label-dsc ;; this means local labels shadow parent labels
        (env-find-label (:env-parent env) sym))))
  (env-set-label!
     [env sym label-class rv-type rdo rv-doc]
      (env-set-label! (:local-sym-table env) sym label-class rv-type rdo rv-doc
                     (env-address env)))
  (env-set-label!
     [env sym label-class rv-type rdo rv-doc address]
       (env-set-label! (:local-sym-table env)
                      sym label-class rv-type rdo rv-doc address))
  (env-set-parent-label!
    [env sym label-class rv-type rdo rv-doc]
     (env-set-label! (:env-parent env) sym label-class rv-type rdo rv-doc
                    (env-address env)))
  (env-set-parent-label!
      [env sym label-class rv-type rdo rv-doc address]
       (env-set-label! (:env-parent env) sym label-class rv-type rdo rv-doc address))

  ;; (env-get-label-addr [env sym])
  ;; (env-set-label-addr [env sym] [env sym address])
  (env-find-type [env sym]
    (let [type-dsc (env-find-type (:local-sym-table env) sym)]
      (if  type-dsc
        type-dsc ;; this means local types shadow parent labels
        (env-find-type (:env-parent env) sym))))
  (env-set-type! [env sym base len arr-base field-map size rv-doc]
    (env-set-type! (:local-sym-table env)
                  sym base len arr-base field-map size rv-doc ))
  (env-set-parent-type! [env sym base len arr-base field-map size rv-doc]
    (env-set-type! (:env-parent env)
                  sym base len arr-base field-map size rv-doc ))
  )

(defn make-local-env
  "Create a new LocalEnv record with a parent environment of `parent`."
  [parent]
  (->LocalEnv parent (make-sym-table false)))

                    ;;;;;;;; User Interface ;;;;;;;;

(def ^:dynamic *env* "Current assembly environment" nil)
(defn pc "Get current Program counter (environment address) value"
  [] (if *env* (env-address *env*) 0))
;; (defn origin [] (env-address *env*))

        ;;;; Assembly instruction Errors ;;;;

(defn asm-ex
  "Assembly exception: Throws an exception which includes `addr` in the `msg`
   preamble."
  ([addr msg]
   (threxf (str "Error at (pc=#X%08X) " msg) addr))
  ([addr msg & args]
   (apply threxf (list* (str "Error at (pc=#X%08X) " msg) addr args))))

(defn cpc-ex
  "Current program counter exception: throws an assembly exception with
  the current program counter as the output address"
  ([msg] (asm-ex (pc) msg ))
  ([msg & args] (apply asm-ex (list* (pc) msg  args))))

;; (defn exrv
;;   ([msg] (exrv msg () (pc)))
;;   ([msg arg-vect] (exrv msg arg-vect (pc)))
;;   ([msg arg-vect addr]
;;    (apply threxf (list* (str "Error at (pc=#X%08X) " msg) addr arg-vect))))

;; (defn exrv
;;   ([msg] (threxf (str "Error at (pc=#X%08X) " msg) (pc)))
;;    ([msg {:keys [addr]}]
;;     (threxf (str "Error at (pc=#X%08X) " msg) addr))
;;    ([msg  arg-vect]
;;     (apply threxf (list* (str "Error at (pc=#X%08X) " msg) (pc) arg-vect)))
;;    ;; ([msg & args]
;;     ;; (apply threxf (list* (str "Error at (pc=#X%08X) " msg) (pc) args)))
;;   ([msg & {:keys [addr]} & args ]
;;   (apply threxf (list* (str "Error at (pc=#X%08X) " msg) addr args)) ))

        ;;;; emit to the environment code vector ;;;;


(defn emit!
  "Emit an `obj` of size `size-key` into to the code vector.
  `size-key` should be one of :byte :kait :vait :zait :yait."
  [size-key & objs]
  (assert (contains? #{:byte :kait :vait :zait :yait} size-key)
          "emit!: `size-key` should be one of :byte :kait :vait :zait :yait")
  (doseq [obj objs] (env-emit! *env* (byte-rev size-key obj))))


;; shortcuts
(defn emit-byte! [& bytes] (apply emit! :byte bytes))
(defn emit-kait! [& kaits] (apply emit! :kait kaits))
(defn emit-vait! [& vaits] (apply emit! :vait vaits))
(defn emit-zait! [& zaits] (apply emit! :zait zaits))
(defn emit-yait! [& yaits] (apply emit! :yait yaits))

(defn advance-to!
  "Increase the program counter to `ofst`, filling the code vector with `fill-byte`.
  `fill-byte` defaults to 0xff."
  ([ofst]
   (advance-to! [ofst 0xff]))
  ([ofst fill-byte]
   (let [addr (env-address *env*) delta (- ofst addr)]
     (when (< delta 0)
       (threxf
        "Cannot advance to %X, it is less than the current environment address %X."
        ofst addr))
     (env-emit! *env* (vec (repeat delta fill-byte))))))


(defn align
  "Align the code vector to the next address divisible by `alignment`,
   filling the code vector with `fill-byte`. `fill-byte` defaults to 0xff."
  ([alignment] (align alignment 0xff))
  ([alignment fill-byte]
   (advance-to! (* alignment (Math/ceil (/ (env-address *env*) alignment)))
                fill-byte)))

        ;;;; labels ;;;;

;; (defn label-val
;;   ;; TODO:
;;   ([])
;;   ([  ])
;;   ([] ))


(defn ptr
  "Within an assembly enviroment `env`, either returns
  the position pointed to by `label-name` (or `label-name` + `offset`),
  or a placeholder with the same `label-name` as the provided pointer."
  [label-name & {:keys [offset env] :or {offset 0 env *env*}}]
  (assert
   ;; (not (nil? env))
   env "ptr: No environment found!")
  (hold-expr label-name [offset]
               (+ offset (or (:label-val (env-find-label label-name env))
                             (throw (placeholder-value-exception
                                     (format "Label %s is undefined!" name)))))))


(defn set-label
  ([label-name label-class rv-type]
   (set-label label-name label-class rv-type nil))
  ([label-name label-class rv-type doc-string]
   (set-label label-name label-class rv-type doc-string nil))
  ([label-name label-class rv-type doc-string read-only]
   (set-label label-name label-class rv-type doc-string read-only *env*))
  ([label-name label-class rv-type doc-string read-only env ]
   (env-set-label! env label-name label-class rv-type read-only doc-string ))
  )

(defn set-ptr
  ([label-name]
   (set-ptr label-name nil ))
  ([label-name doc-string]
   (set-ptr label-name doc-string *env*))
  ([label-name doc-string env]
   (set-label label-name :ptr nil doc-string)))

(defn label-difference [start-name end-name]
  (let [start (ptr start-name)
        end (ptr end-name)]
    (hold-expr :label-difference [start end] (- end start))))

(defn label-inc [label-name ofst]
  (let [start (ptr label-name)]
    (hold-expr :label-inc [start] (+ start ofst))))

(defn offset
  "Calculate the offset of `label` from the current address"
  [label]
  (let [addr (pc)]
    (hold-expr :offset [label] (- label addr))))

