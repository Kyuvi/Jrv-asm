(defproject jrv-asm "0.1.0-SNAPSHOT"
  :description "A dynamic RISC-V assembler in clojure"
  ;; :url "http://example.com/FIXME"
  :license {:name "GPL-3.0-or-later WITH Classpath-exception-2.0"
            ;; :url "https://www.eclipse.org/legal/epl-2.0/"
            :url "https://www.gnu.org/licenses/gpl-3.0.html"
            }
  :dependencies [[org.clojure/clojure "1.10.0"]]
  :repl-options {:init-ns jrv.core
                 ;; :init (do (load-file "./src/jrv/korr/cj-utils.clj")
                           ;; (load-file "./src/jrv/korr/env.clj")
                           ;; (load-file "./src/jrv/korr/reg.clj")
                           ;; (load-file "./src/jrv/korr/fmt.clj")
                           ;; (load-file "./src/jrv/korr/io.clj")
                           ;; (refer 'jrv.korr.cj-utils)
                           ;; (refer 'jrv.korr.env)
                           ;; (refer 'jrv.korr.reg)
                           ;; (refer 'jrv.korr.fmt)
                           ;; (refer 'jrv.korr.io)
                          ;; )
                 }
  :source-paths ["src" "src/jrv" "src/jrv/korr"] ; "src/ins_jrv"}
)
