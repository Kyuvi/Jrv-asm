(ns jrv.core-test
  (:require [clojure.test :refer :all]
            ;; [jrv.core :refer :all]
            )
  (:require [jrv.korr
             [cj-utils :refer :all]
             [env :refer :all]
             [reg :refer :all]
             [fmt :refer :all]
             ])
  )


(defmacro with-test-env [& body]
  ;; (let [*env* (make-basic-env)
         ;; *max-address* 4000]
  ;; (with-bindings {*env* (make-basic-env)
         ;; *max-address* 4000}
  `(binding [*env* (make-basic-env)
         *max-address* 4000]
     ;; `(do
        ~@body))
  ;; )

(defn cd-vec [env] (:env-code-vec (:code-vec env)))

(defn take-env-bytes [num]
  (let [cvec @(:env-code-vec (:code-vec *env*))]
    ;; (loop [rvec [] itr num]
          ;; (if (< itr 0)
            ;; rvec
            ;; (recur (conj rvec (cvec itr)) (dec itr)))))
      (take num cvec))
  )

(defn get-first-env-byte []
  (take-env-bytes 1))

(defn get-first-env-kait []
  (take-env-bytes 2))

(defn get-first-env-vait []
  (take-env-bytes 4))

(defn get-first-env-zait []
  (take-env-bytes 8))

(defn rebuild-ins [siiq]
  ;; (let [size-num ({:kait 2 :vait 4} size)
        ;; ins-seq (take-env-bytes size-num)]
   (let [size-num (count siiq)]
    (if (== size-num 2)
      (let [[a b] siiq]
        (+ (bit-shift-left b 8) a))
      (let [[a b c d] siiq]
        (+ (bit-shift-left d 24) (bit-shift-left c 16) (bit-shift-left b 8) a)))))


(defmacro jrv-c= [form nom]
  `(= (with-test-env ~form (rebuild-ins (take-env-bytes 2))) ~nom))

(defmacro jrv-v= [form nom]
  `(= (with-test-env ~form (rebuild-ins (take-env-bytes 4))) ~nom))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))
    (is (= 1 2))))
