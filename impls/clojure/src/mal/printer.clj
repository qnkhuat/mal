(ns mal.printer)

(declare pr-str)

(defn pr-str [l] 
  (cond 
    (vector? l) (str "[" (clojure.string/join " " (map pr-str l)) "]")
    (list? l) (str "(" (clojure.string/join " " (map pr-str l)) ")")
    (symbol? l) l
    (number? l) l
    (boolean? l) (str l)
    true (str "\"" l "\"")
  ))
