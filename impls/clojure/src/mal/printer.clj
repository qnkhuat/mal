(ns mal.printer
  (:use [clojure.test :only [function?]])
  )

(declare pr-str)
(defn escape [s]
  (-> s (clojure.string/replace "\\" "\\\\")
      (clojure.string/replace "\"" "\\\"")
      (clojure.string/replace "\n" "\\n")))

(defn pr-str 
  ([l] (pr-str l true))
  ([l readably] (cond 
    (vector? l) (str "[" (clojure.string/join " " (map #(pr-str % readably) l)) "]")
    (or (list? l) (seq? l)) (str "(" (clojure.string/join " " (map #(pr-str % readably) l)) ")")
    (map? l) (str "{" (clojure.string/join " " (map (fn [[k v]] (str (#(pr-str % readably) k) " " (#(pr-str % readably) v))) l)) "}")
    (or (symbol? l) (number? l) (keyword? l)) l
    (= (type l) clojure.lang.Atom) (str "(atom " (pr-str @l readably) ")")
    (boolean? l) (str l)
    (nil? l) "nil"
    (string? l) (if readably (str "\"" (escape l) "\"") l)
    (function? l) "#<function>"
    :else (throw (Exception. (str "Invalid data type: " (type l) l)))))
  )
