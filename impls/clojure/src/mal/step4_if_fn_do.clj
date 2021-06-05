(ns mal.step4-if-fn-do
  (:require 
    [clojure.repl]
    [mal.reader :as reader]
    [mal.printer :as printer]
    [mal.env :as env]
    [mal.core :as core]
    :reload)
  (:gen-class))

; Eval
(def repl-env (env/env))
; Add primitives function
(doall (map (fn [[k v]] (env/env-set repl-env k v)) 
            core/ns-core))

(declare EVAL)
(defn eval-ast [ast env]
  (cond 
    (symbol? ast) (let [sym (env/env-get env ast)]
                    (if (nil? sym)
                      (throw (Exception. (format "Symbol %s not found" ast)))
                      sym))
    (list? ast)  (map #(EVAL % env) ast)
    (vector? ast) (vec (map #(EVAL % env) ast))
    (map? ast) (reduce-kv (fn [new-map k v] (assoc new-map k (EVAL v env))) {} ast)
    :else ast ))

(defn READ [s] (reader/read-str s))

; TODO have safe access to elements for def! and let*

(defn EVAL [ast env] 
  (if (list? ast)
    (if (empty? ast)
      ast
      (let [[a0 a1 a2 a3] ast]
        (cond (= (symbol "def!") a0) (env/env-set env a1 (EVAL a2 env))
              (= (symbol "do") a0) (last (map (fn [x] (EVAL x env)) (rest ast)))
              (= (symbol "if") a0) (let [[_ predicate true-ast false-ast] ast]
                                     (if (EVAL predicate env)
                                       (EVAL true-ast env)
                                       (EVAL false-ast env)))
              (= (symbol "fn*") a0) (fn [& args]
                                      (EVAL a2 (env/env env a1 (apply list args)))) ; This is where program recursively create env which might lead to stackoverflow
              (= (symbol "let*") a0) (let [let-env (env/env env)]
                                       (do
                                         (doall (map (fn [[k v]] (env/env-set let-env k (EVAL v let-env))) (partition 2 a1)))
                                         (EVAL a2 let-env)
                                         ))
              :else (let [evaluated-list (eval-ast ast env)]
                        (apply (first evaluated-list) (rest evaluated-list))
                        ))))
    (eval-ast ast env)
    ))


(defn PRINT [l] (println (printer/pr-str l)))

(defn rep [s] (PRINT (EVAL (READ s) repl-env)))

(rep "(def! not (fn* [a] (if a false true)))") ; Define not using mal itself

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

