(ns mal.readlnie
  (:require [net.n01se.clojure-jna :as jna]))

(def readline (jna/to-fn String edit/readline))

(do
  (def readline-call (jna/to-fn String edit/readline))
  (def add-history (jna/to-fn Void edit/add_history))
  (def load-history #(doseq [line (split (slurp %) #"\n")]
                       (jna/invoke Void edit/add_history line))))
