(defproject mal "0.1.0-SNAPSHOT"
  :description "Make a Lisp in Clojure"
  
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [net.n01se/clojure-jna "1.0.0"]]

  ;; To generate a executable uberjar (in target/) for a step:
  ;; lein with-profile stepX repl
  :profiles {:step0 {:main mal.step0-repl
                     :uberjar-name "step0_repl.jar"
                     :aot [mal.step0-repl]}
             })
