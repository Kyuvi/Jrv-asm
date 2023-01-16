(ns jrv.korr.cj-utils
 "Clojure utils for the jrv RISC-V assembler."
 {:author "Kyuvi"
  :license {:name "GPL-3.0 WITH Classpath-exception-2.0"
            :url "https://www.gnu.org/licenses/gpl-3.0.html"}}
  )


(defn threxf
;; (defn Exftr
  "Throw an exception with (format) message `msg` and  arguments `args`"
  [msg & args]
  (throw (Exception. (apply format msg args))))


(defn symbol-append [& args]
  (assert (every? symbol? args)
          "symbol-append: All arguments should be keywords")
   (symbol (apply str (map name args)))
    )

(defn keyword-append [& args]
  (assert (every? keyword? args)
          "keyword-append: All arguments should be keywords")
   (keyword (apply str (map name args)))
    )


;; (defn promise? [p]
;;   (isa? (class p) clojure.lang.Ipending))
;; ;;  (instance? clojure.lang.Ipending p ))
;; ;  (every? #(instance % p)
;; ;          [clojure.lang.Ipending clojure.lang.IFn
;; ;           clojure.lang.IBlockingDeref clojure.lang.IDeref]))

;; (defn pend-type? [p]
;;   (isa? (class p) clojure.lang.Ipending))

(defn str-bytes-list
   ([string]
     (str-bytes-list string :clj))
   ([string encodeing]
    (let [str-list (map int string)]
      (case encodeing
        (:a :ascii)
        (do
        (assert (every? (fn [x] (< x 128)) str-list)
            "str-bytes-list: not all ascii characters")
         str-list)
        (:clj :clojure :norm) str-list
        :usb-ucs :usb
        (apply concat (map (fn [x] (list
                                    (bit-and 0xff x)
                                    (bit-and 0xff (bit-shift-right x 8))))
                          str-list))
        (:ucs2 :ucs)
        (apply concat (map (fn [x] (list
                                   (bit-and 0xff (bit-shift-right x 8))
                                   (bit-and 0xff x)
                                   ))
                             str-list))
    ))))

        ;;;; Numbers/Integers ;;;;
;; (defn int-expt [n p]
  ;; (.pow (biginteger n) p))


(defprotocol IntegerProtocol ;; Int protocol
  (int-expt [n p]
    "Integer Exponent: Returns integer `n` raised to the power of (floor`p`).")
  (bit-field [x a] [x a b]
    "Returns the value of the bit(s) in `x` either at point `a`
     or between `a` and `b` (inclusive)"))
  ;; len and or xor not shift (unsigned?\) test clear(?)

(extend-protocol IntegerProtocol
  java.lang.Long
  (int-expt [n p] (.pow (biginteger n) p))
  (bit-field
    ([x a] (bit-and (bit-shift-right x  a) 1))
    ([x a b]
     (let [low (min a b) high (max a b)]
       (bit-and (bit-shift-right x low)
                      (dec (bit-shift-left 1 (- high low -1)))))))

  clojure.lang.BigInt
  (int-expt [n p] (.pow (biginteger n) p))
  (bit-field
    ([x a] (bit-field (biginteger x) a))
     ;; (bit-field (biginteger x) (biginteger a)))
    ([x a b] (bit-field (biginteger x) a b)))
     ;; (bit-field (biginteger x) (biginteger a) (biginteger b)))))

  BigInteger
  (int-expt [n p] (.pow n p))
  (bit-field
    ([x a] (.and (.shiftRight x a) (biginteger 1)))
    ([x a b]
     (let [low (min a b ) high (max a b)]
       (.and (.shiftRight x low)
             (biginteger (dec (.shiftLeft (biginteger 1)
                                          (- high low -1)))))))))
                       ;; (biginteger (- (biginteger a) (biginteger b) -1) )))))


(defn unsigned-int?
  "Test if `x` can fit into `bit-len` bits (unsigned).
   That is if `x` is an integer between 0 and (- 2^`bit-len` 1) inclusive."
  [x bit-len]
  (and (integer? x) (<= 0 x (dec
                             ;; (.pow (biginteger 2) bit-len)))))
                             (int-expt  2 bit-len)))))

(defn signed-int?
  "Test if `x` can fit into `bit-len` bits (2's compliment).
   That is if `x` is an integer between
    -2^`(- bit-len 1)` and (- 2^`(- bit-len 1)` 1) inclusive."
  [x bit-len]
  (and (integer? x)
       (<= (- (int-expt 2 (dec bit-len)) x (dec (int-expt 2 (dec bit-len)))))))

(defn twos-complement-int
  "Return the twos complement of the number `numa` within the range of `bit-len`"
  [numa bit-len]
  {:pre [(and (integer? numa)
         (<= (.bitLength (biginteger numa)) bit-len))]}
  ;; (let [sign-bit (bit-test numa (- bit-len 1))]
  (let [sign-bit (.testBit (biginteger numa) (- bit-len 1))]
    (cond
            ;; positive numbers are ok if less than register-length
            (clojure.core/and  (false? sign-bit) (pos? numa))
             numa
            ;; if sign bit is set for positive numbers, complement
             (clojure.core/and (true? sign-bit) (pos? numa))
             ;; (- (.xor (biginteger (dec (Math/pow 2 bit-len)))
                      ;; (biginteger (dec numa ))))
             (- (.xor (biginteger (dec (int-expt 2 bit-len)))
                      (biginteger (dec numa))))
            ;; large negative numbers whose complement bits are higher than the
            ;; MSB of the register-length in clojure number representation
            ;; need to be complemented as well by adding them to max bit-length + 1
            (clojure.core/and  (false? sign-bit) (neg? numa))
             ;; (+ (biginteger (Math/pow  2 bit-len)) numa)
             (+ (int-expt 2 bit-len) numa) ;; Math/pow approximates
            ;; negative numbers smaller than register length
            ;; are already complemented properly
            (clojure.core/and sign-bit (neg? numa))
             numa
             :else (throw
                   (Exception.
                     (format
                     "twos-complement-int: Error with number %s ." numa)))
    )))

(defn tcv
  "Return the 32-bit twos-complement representation of `numa`"
  [numa]
 (long (twos-complement-int numa 32)))

(defn tcz
  "Return the 64-bit twos-complement representation of `numa`"
  [numa]
    (twos-complement-int numa 64))
