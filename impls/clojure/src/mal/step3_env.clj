(ns mal.step3-env
  (:require 
    [clojure.repl]
    [mal.reader :as reader]
    [mal.printer :as printer]
    [mal.env :as env]
    :reload)
  (:gen-class))

; Eval
(def repl-env (env/env-create))
; Add primitives function
(doall (map (fn [[k v]] (env/env-set repl-env k v)) 
            {(symbol '+) + 
             (symbol '-) - 
             (symbol '*) * 
             (symbol '/) /}))

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
(defn EVAL [ast env] ast
  (if (list? ast)
    (if (empty? ast)
      ast
      (cond (= (symbol "def!") (first ast)) (env/env-set env (second ast) (EVAL (nth ast 2) env))
            (= (symbol "let*") (first ast)) (let [[ _ let-bind let-eval ] ast
                                                  let-env (env/env-create env)]
                                              (do
                                                (doall (map (fn [[k v]] (env/env-set let-env k (EVAL v let-env))) (partition 2 let-bind)))
                                                (EVAL let-eval let-env)))
            :else (let [evaluated-list (eval-ast ast env)]
                    (apply (first evaluated-list) (rest evaluated-list)))))
    (eval-ast ast env)
    ))

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


