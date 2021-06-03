(ns mal.step1-read-print
  (:require 
    [clojure.repl]
    [mal.reader :as reader]
    [mal.printer :as printer])
  (:gen-class))


(defn READ [s] (reader/read_str s))

(defn EVAL [l] l)

(defn PRINT [l] (println (printer/pr_str l)))

(defn rep [s] (PRINT (EVAL (READ s))))

(defn -main [] 
  (do
    (print "user> ")
    (flush)
    (let [line (read-line)]
      (if (nil? line)
        (System/exit 0)
        (do
          (try 
            (rep line)
            (catch Throwable e (clojure.repl/pst e)))
          (-main)
          )))))
