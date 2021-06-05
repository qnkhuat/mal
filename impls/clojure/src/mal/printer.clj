(ns mal.printer
  (:use [clojure.test :only [function?]])
  )

(declare pr-str)

(defn escape [s]
  (-> s (clojure.string/replace "\\" "\\\\")
      (clojure.string/replace "\"" "\\\"")
      (clojure.string/replace "\n" "\\n")))

(defn pr-str [l] 
  (cond 
    (vector? l) (str "[" (clojure.string/join " " (map pr-str l)) "]")
    (list? l) (str "(" (clojure.string/join " " (map pr-str l)) ")")
    (map? l) (str "{" (clojure.string/join " " (map (fn [[k v]] (str (pr-str k) " " (pr-str v))) l)) "}")
    (or (symbol? l) (number? l) ) l
    (boolean? l) (str l)
    (nil? l) "nil"
    (string? l) (str "\"" (escape l) "\"")
    (function? l) "#<function>"
    :else (throw (Exception. (str "Invalid data type: " l))))
  )
