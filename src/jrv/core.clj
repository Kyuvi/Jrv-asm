(ns jrv.core
  ;; (:require [jrv.korr.cj-utils :refer :all] :reload)
  ;; (:require [jrv.korr.env :refer :all] :reload)
  ;; (:require [jrv.korr.reg :refer :all] :reload)
  ;; (:require [jrv.korr.cj-utils :refer :all]
  ;;           [jrv.korr.env :refer :all]
  ;;           [jrv.korr.reg :refer :all]
  ;;           :reload-all)
  (:require [jrv.korr
             [cj-utils :refer :all]
             [env :refer :all]
             [reg :refer :all]
             ;; [fmt :refer :all]
             ;; [io :refer :all]
             ]
            :reload-all
            )
  )

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
