(ns mal.printer)

(declare pr-str)

(defn pr-str [l] 
  (cond 
    (vector? l) (str "[" (clojure.string/join " " (map pr-str l)) "]")
    (list? l) (str "(" (clojure.string/join " " (map pr-str l)) ")")
    (map? l) (str "{" (clojure.string/join " " (map (fn [[k v]] (str (pr-str k) " " (pr-str v))) l)) "}")
    (symbol? l) l
    (number? l) l
    (boolean? l) (str l)
    true (str "\"" l "\"")
  ))
