(ns mal.printer)

(declare pr-str)

(defn pr-str [l] 
  (cond 
    (or (vector? l) (list? l)) (str "(" (clojure.string/join " " (map pr-str l)) ")")
    (symbol? l) l
    (number? l) l
    (boolean? l) (str l)
    true (str "\"" l "\"")
  ))
