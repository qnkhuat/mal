(ns mal.step7-quote
  (:require 
    [clojure.repl]
    [mal.reader :as reader]
    [mal.printer :as printer]
    [mal.env :as env]
    [mal.core :as core]
    :reload)
  (:gen-class))

; Eval
(declare EVAL)
(def repl-env (env/env))
; Add primitives function
(doall (map (fn [[k v]] (env/env-set repl-env k v)) 
            core/ns-core))
(env/env-set repl-env 'eval  (fn [ast] (EVAL ast repl-env)))
(env/env-set repl-env '*ARGV* '())

(defn eval-ast [ast env]
  (cond 
    (symbol? ast) (let [sym (env/env-get env ast)]
                    (if (nil? sym)
                      (throw (Exception. (format "Symbol %s not found" ast)))
                      sym))
    (list? ast)  (map #(EVAL % env) ast)
    (vector? ast) (vec (map #(EVAL % env) ast))
    (map? ast) (reduce-kv (fn [new-map k v] (assoc new-map k (EVAL v env))) {} ast)
    :else ast))

(defn READ [s] (reader/read-str s))

; TODO have safe access to elements for def! and let*
; NOTE on TCO:
; I've ran some analysis but found not much of different between this version and the step4 versino
; Even though this version show no stackoverflow when run a big recursive tail call
; My weak hypothesis is using this version with loop, we don't init to call EVAL many times, hence locates less 
; memory. But I doubt the call of EVAl in step4 is stay in memory during execution.
; Need a better tool for analysis

(def debug (atom {:v nil}))

(defn EVAL [ast env] 
  (loop [ast ast 
         env env]
    (if-not (list? ast)
      (eval-ast ast env)
      (if (empty? ast)
        ast
        (let [[a0 a1 a2 a3] ast]
          (cond (= 'def! a0) (env/env-set env a1 (EVAL a2 env))
                (= 'do a0) (do
                                       (doall (map (fn [x] (EVAL x env)) (drop-last (rest ast)))) ; Evaluate ast[1:-1]
                                       (recur (last ast) env))
                (= 'if a0) (if (EVAL a1 env)
                                       (recur a2 env) ; TCO 
                                       (recur a3 env)) ; TCO
                (= 'fn* a0) (with-meta (fn [& args] ; This is where program recursively create env which might lead to stackoverflow
                                                   (EVAL a2 (env/env env a1 (apply list args))))
                                                 {:expression a2
                                                  :params a1
                                                  :environment env})
                (= 'let* a0) (let [let-env (env/env env)]
                                         (do
                                           (doall (map (fn [[k v]] (env/env-set let-env k (EVAL v let-env))) (partition 2 a1)))
                                           (recur a2 let-env) ; TCO
                                          ))
                :else (let [evaluated-list (eval-ast ast env)
                            f (first evaluated-list)
                            args (rest evaluated-list)
                            {:keys [expression params environment]} (meta f)]
                        (if expression
                           (recur expression (env/env environment params args)) ; TCO
                            (apply f args)
                          ))))))))


(defn PRINT [o] (println (printer/pr-str o)))

(defn re [s] (EVAL (READ s) repl-env))
(defn rep [s] (PRINT (re s)))

(re "(def! not (fn* [a] (if a false true)))") ; Define not using mal itself
(re "(def! load-file (fn* [f] (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))")

(defn -main [& args] 
  (env/env-set repl-env '*ARGV* (rest args))
  (if args
    (re (str "(load-file \"" (first args) "\")"))
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
          )))))

