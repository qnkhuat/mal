(ns mal.core
  (:require 
    [mal.printer :as printer]
    [mal.reader :as reader]
  ))


(defn mal_throw [o]
  (throw (ex-info "Exception" {:data o})))

(def ns-core {'+ + 
             '- - 
             '* * 
             '/ /
             'list list
             'list? list?
             'empty? empty?
             'count count
             '= =
             '< <
             '<= <=
             '> >
             '>= >=
             'prn (fn [& xs] (println (clojure.string/join " " (map #(printer/pr-str % true) xs))))
             'str (fn [& xs] (clojure.string/join "" (map #(printer/pr-str % false) xs)))
             'pr-str (fn [& xs] (clojure.string/join " " (map #(printer/pr-str % true) xs)))
             'println (fn [& xs] (println (clojure.string/join " " (map #(printer/pr-str % false) xs))))
             'atom atom
             'atom? (fn [atm] (= (type atm) clojure.lang.Atom))
             'reset! reset!
             'swap! swap! 
             'deref deref
             'read-string reader/read-str
             'slurp slurp
             'cons cons
             'concat #(apply list (apply concat %&))
             'vec vec
             'nth nth
             'first first
             'rest rest
             'last last
             'throw mal_throw
             'nil? nil?
             'true? true?
             'false? false?
             'symbol symbol
             'symbol? symbol?
             'vector? vector? 
             'vector vector
             'keyword keyword
             'keyword? keyword?
             'hash-map hash-map
             'map? map?
             'assoc assoc
             'dissoc dissoc
             'get get
             'contains? contains?
             'keys (fn [hm] (let [ks (keys hm)] (if (nil? ks) '() ks)))
             'vals (fn [hm] (let [vs (vals hm)] (if (nil? vs) '() vs)))
             'sequential? sequential?
             'apply apply
             'map map
             })

