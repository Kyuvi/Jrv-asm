(ns jrv.korr.utils
  "Core utilities for the jrv RISC-V assembler."
   {:author "Kyuvi"
    :license {:name "GPL-3.0 WITH Classpath-exception-2.0"
              :url "https://www.gnu.org/licenses/gpl-3.0.html"}}

  (:require [jrv.korr
             [cj-utils :refer :all]
             [env :refer :all]
             [reg :refer :all]
             ;; [fmt :refer :all]
             ]
  ))

(defn set-addressed-label
  ([nam label-class rv-type address ]
   (set-label name label-class rv-type nil address))
  ([name label-class rv-type doc-string address]
   (set-label name label-class rv-type doc-string nil))
  ([name label-class rv-type doc-string read-only address]
   (set-label name label-class rv-type doc-string read-only *env*))
  ([nam label-class rv-type doc-string read-only address env]
   (env-set-label! env sym label-class rv-type rdo rv-doc address)))

(defun weak-ptr (nam backup & {:keys [ofst env] :or {ofst 0 env *env*}})
  ;; needs to be used before the backup is defined or
  ;; backup needs to be defined after useful name labels
  (assert (not (nil? env)) "weak-ptr: No environment found!" )
  (hold-expr name (ofst)
               (+ ofst (or (env-find-label env nam)
                           (progn (warn "using label ~a instead of ~a" backup nam)
                                  (ptr backup))))))




(defmacro with-ptr
  [label & body]
  (when (and (list? label) (= (first label) 'quote))
    (format "with-ptr: Quoted label name %s probably not what you intended"
            label))
  `(do (set-ptr label) ~@body))

(defmacro usoro [name & body]
  (let [doc-string (when (string? (first-body))
                     (format "`Simple jrv procedure`\n%s" (first body)))
        proc-body (if doc-string (rest body) body)
        label-exp (if doc-string
                    `(set-label ~name :simple-jrv-proc nil ~doc-string)
                    `(set-label ~name :simple-jrv-proc nil))
        ]
    `(do
       ~label-exp
       (let [*env* (make-local-env *env*)]
         ~@body)))
  )
