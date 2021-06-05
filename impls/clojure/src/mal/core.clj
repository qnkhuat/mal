(ns mal.core
  (:require 
    [mal.printer :as printer]
    [mal.reader :as reader]
  ))

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
             })

