(ns mal.step2-eval
  (:require 
    [clojure.repl]
    [mal.reader :as reader]
    [mal.printer :as printer]
    :reload)
  (:gen-class))

; Eval
(def repl-env {(symbol '+) + 
               (symbol '-) - 
               (symbol '*) * 
               (symbol '/) /})

(declare EVAL)

(defn eval-ast [ast env]
  (cond 
    (symbol? ast) (let [sym (get env ast nil)]
                    (if (nil? sym)
                      (throw (Exception. (format "Symbol %s not found" ast)))
                      sym))
    (list? ast)  (map #(EVAL % env) ast)
    (vector? ast) (vec (map #(EVAL % env) ast))
    (map? ast) (reduce-kv (fn [new-map k v] (assoc new-map k (EVAL v env))) {} ast)
    :else ast ))

(defn READ [s] (reader/read-str s))

(defn EVAL [ast, env]
  (if (list? ast)
    (if (empty? ast)
      ast
      (let [evaluated-list (eval-ast ast env)]
        (apply (first evaluated-list) (rest evaluated-list))))
    (eval-ast ast env)
    )
  )

(defn PRINT [l] (println (printer/pr-str l)))

(defn rep [s] (PRINT (EVAL (READ s) repl-env)))

(defn -main [] 
  (loop []
    (do
      (print "user> ")
      (flush)
      (let [line (read-line)]
        (if (nil? line)
          (System/exit 0)
          (try 
            (rep line)
            (catch Throwable e (clojure.repl/pst e))))
        (recur)
        ))))

