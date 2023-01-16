(ns
  ^{:author "Kyuvi"
    :doc "Functions to read and write to and from the jrv RISC-V assembler."
     }
    jrv.korr.io
    [:require [clojure.java.io :refer [file input-stream output-stream]]])

(defn write-binary-file [file code-vec]
 (with-open [out (output-stream (file file-str))]
  (.write out (byte-array code-vec)))

(defn read-binary-file [file-str]
  (let [infile (file file-str)
        file-len (.length infile)
        buf (byte-array file-len)]
    (with-open [in (input-stream infile)]
      (.read in buf))
      (vec buf))))
