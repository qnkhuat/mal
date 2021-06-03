(ns mal.printer)

(declare pr_str)

(defn pr_str [l] 
  (cond 
    (or (vector? l) (list? l)) (str "(" (clojure.string/join " " (map pr_str l)) ")")
    true (str "\"" l "\"")
  ))
