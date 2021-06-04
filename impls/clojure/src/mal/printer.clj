(ns mal.printer)

(declare pr-str)

(defn pr-str [l] 
  (cond 
    (vector? l) (str "[" (clojure.string/join " " (map pr-str l)) "]")
    (list? l) (str "(" (clojure.string/join " " (map pr-str l)) ")")
    (map? l) (str "{" (clojure.string/join " " (doseq [[k v] l] (str (pr-str k) " " (pr-str v))) "}") ; REFINE THIS
    (symbol? l) l
    (number? l) l
    (boolean? l) (str l)
    true (str "\"" l "\"")
  ))

