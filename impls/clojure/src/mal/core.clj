(ns mal.core
  (:require 
    [mal.printer :as printer]
    [mal.reader :as reader]
  ))

(def ns-core {(symbol '+) + 
             (symbol '-) - 
             (symbol '*) * 
             (symbol '/) /
             (symbol 'list) list
             (symbol 'list?) list?
             (symbol 'empty?) empty?
             (symbol 'count) count
             (symbol '=) =
             (symbol '<) <
             (symbol '<=) <=
             (symbol '>) >
             (symbol '>=) >=
             (symbol 'prn) (fn [& xs] (println (clojure.string/join " " (map #(printer/pr-str % true) xs))))
             (symbol 'str) (fn [& xs] (clojure.string/join "" (map #(printer/pr-str % false) xs)))
             (symbol 'pr-str) (fn [& xs] (clojure.string/join " " (map #(printer/pr-str % true) xs)))
             (symbol 'println) (fn [& xs] (println (clojure.string/join " " (map #(printer/pr-str % false) xs))))
             (symbol 'atom) atom
             (symbol 'atom?) (fn [atm] (= (type atm) clojure.lang.Atom))
             (symbol 'reset!) reset!
             (symbol 'swap!) swap! 
             (symbol 'deref) deref
             (symbol 'read-string) reader/read-str
             (symbol 'slurp) slurp
             })

