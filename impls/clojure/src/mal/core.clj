(ns mal.core
  (:require 
    [mal.printer :as printer]
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
             })

